{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
-- | Volatile on-disk database of binary blobs
--
-- = Logic
--
-- The db is a key-value store of binary blocks and is parametric on the key
-- of blocks, named @blockId@.
--
-- The database uses in memory indexes, which are created on each reopening.
-- reopening includes parsing all blocks of the dbFolder, so it can be an
-- expensive operation if the database gets big. That's why the intention of
-- this db is to be used for only the tip of the blockchain, when there is still
-- volatility on which blocks are included. The db is agnostic to the format of
-- the blocks, so a parser must be provided. In addition to getBlock and
-- putBlock, the db provides also the ability to garbage-collect old blocks.
-- The actual garbage-collection happens in terms of files and not blocks: a
-- file is deleted/garbage-collected only if its latest block is old enough. A
-- block is old enough if its toSlot value is old enough and not based on its
-- Ord instance. This type of garbage collection makes the deletion of blocks
-- depend on the number of blocks we insert on each file, as well as the order
-- of insertion, so it's not deterministic on blocks themselves.
--
-- = Errors
--
-- On any exception or error the db closes and its Internal State is lost,
-- inluding in memory indexes. We try to make sure that even on errors the
-- fs represantation of the db remains consistent and the Internal State
-- can be recovered on reopening. In general we try to make sure that at
-- any point, losing the in-memory Internal State is not fatal to the db
-- as it can recovered. This is important since we must always expect unexpected
-- shutdowns, power loss, sleep mode etc.
-- This is achived by leting only basic operations on the db:
-- + putBlock only appends a new block on a file. Losing an update means we only
--   lose a block, which can be recovered.
-- + garbage collect deletes only whole files.
-- + there is no modify block operation. Thanks to that we need not keep any
--   rollback journals to make sure we are safe in case of unexpected shutdowns.
--
-- We only throw VolatileDBError. All internal errors, like io errors, are
-- cought, wrapped and rethrown. For all new calls of HasFs, we must make sure
-- that they are used properly wrapped. All top-level function of this module
-- are safe. You can safely use HasFs calls in modifyState or wrapFsError
-- actions.
--
-- = Concurrency
--
-- The same db should only be opened once
-- Multiple threads can share the same db as concurency if fully supported.
--
-- = FS Layout:
--
-- On disk represantation is as follows:
--
--  dbFolder\
--    blocks-0.dat
--    blocks-1.dat
--    ...
--
--  If on opening any other filename which does not follow blocks-i.dat is found
--  an error is raised. The Ordering of blocks is not guarranteed to be
--  followed, files can be garbage-collected.
--
--  Each file stores a fixed number of slots, specified by _maxBlocksPerFile.
--  If the db finds files with less blocks than this max, it will start
--  appending to the newest of them, if it's the newest of all files. If it's
--  not the newest of all files it will create a new file to append blocks..
--
--  There is an implicit ordering of block files, which is NOT alpharithmetic
--  For example blocks-20.dat < blocks-100.dat
--
-- = Recovery
--
-- The VolatileDB will always try to recover to a consistent state even if this
-- means deleting all of its contents. In order to achieve this, it truncates
-- the files containing blocks if some blocks fail to parse, are invalid, or are
-- duplicated. The db ignores files with unrecognised names.
--
module Ouroboros.Consensus.Storage.VolatileDB.Impl
    ( -- * Opening a database
      openDB
    ) where

import           Control.Monad
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Builder as BS
import           Data.List (foldl', sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.Util.ErrorHandling
                     (ErrorHandling (..), ThrowCantCatch (..))
import qualified Ouroboros.Consensus.Storage.Util.ErrorHandling as EH
import           Ouroboros.Consensus.Storage.VolatileDB.API
import           Ouroboros.Consensus.Storage.VolatileDB.FileInfo (FileInfo)
import qualified Ouroboros.Consensus.Storage.VolatileDB.FileInfo as FileInfo
import           Ouroboros.Consensus.Storage.VolatileDB.Index (Index)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
  Main Types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h e. VolatileDBEnv {
      _dbHasFS          :: !(HasFS m h)
    , _dbErr            :: !(ErrorHandling VolatileDBError m)
    , _dbErrSTM         :: !(ThrowCantCatch VolatileDBError (STM m))
    , _dbInternalState  :: !(StrictMVar m (OpenOrClosed blockId h))
    , _maxBlocksPerFile :: !BlocksPerFile
    , _parser           :: !(Parser e m blockId)
    , _tracer           :: !(Tracer m (TraceEvent e blockId))
    }

data OpenOrClosed blockId h =
    VolatileDbOpen !(InternalState blockId h)
  | VolatileDbClosed
  deriving (Generic, NoUnexpectedThunks)

volatileDbIsOpen :: OpenOrClosed blockId h -> Bool
volatileDbIsOpen (VolatileDbOpen _) = True
volatileDbIsOpen VolatileDbClosed   = False

data InternalState blockId h = InternalState {
      _currentWriteHandle :: !(Handle h)
      -- ^ The only open file we append blocks to.
    , _currentWritePath   :: !FsPath
      -- ^ The path of the file above.
    , _currentWriteId     :: !FileId
      -- ^ The 'FileId' of the same file.
    , _currentWriteOffset :: !Word64
      -- ^ The offset of the same file.
    , _currentMap         :: !(Index blockId)
      -- ^ The contents of each file.
    , _currentRevMap      :: !(ReverseIndex blockId)
      -- ^ Where to find each block based on its slot number.
    , _currentSuccMap     :: !(SuccessorsIndex blockId)
      -- ^ The successors for each block.
    , _currentMaxSlotNo   :: !MaxSlotNo
      -- ^ Highest stored SlotNo.
      --
      -- INVARIANT: this is the cached value of:
      -- > FileInfo.maxSlotInFiles (Index.elems (_currentMap st))
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

openDB :: ( HasCallStack
          , IOLike m
          , Ord                blockId
          , NoUnexpectedThunks blockId
          , Typeable           blockId
          , Show               blockId
          )
       => HasFS m h
       -> ErrorHandling VolatileDBError m
       -> ThrowCantCatch VolatileDBError (STM m)
       -> Parser e m blockId
       -> Tracer m (TraceEvent e blockId)
       -> BlocksPerFile
       -> m (VolatileDB blockId m)
openDB hasFS err errSTM parser tracer maxBlocksPerFile = do
    st    <- mkInternalStateDB hasFS err parser tracer maxBlocksPerFile
    stVar <- newMVar $ VolatileDbOpen st
    let env = VolatileDBEnv {
            _dbHasFS          = hasFS
          , _dbErr            = err
          , _dbErrSTM         = errSTM
          , _dbInternalState  = stVar
          , _maxBlocksPerFile = maxBlocksPerFile
          , _parser           = parser
          , _tracer           = tracer
          }
    return VolatileDB {
        closeDB           = closeDBImpl           env
      , isOpenDB          = isOpenDBImpl          env
      , reOpenDB          = reOpenDBImpl          env
      , getBlockComponent = getBlockComponentImpl env
      , putBlock          = putBlockImpl          env
      , garbageCollect    = garbageCollectImpl    env
      , getSuccessors     = getSuccessorsImpl     env
      , getBlockInfo      = getBlockInfoImpl      env
      , getMaxSlotNo      = getMaxSlotNoImpl      env
      }

closeDBImpl :: IOLike m
            => VolatileDBEnv m blockId
            -> m ()
closeDBImpl VolatileDBEnv{..} = do
    mbInternalState <- swapMVar _dbInternalState VolatileDbClosed
    case mbInternalState of
      VolatileDbClosed -> traceWith _tracer DBAlreadyClosed
      VolatileDbOpen InternalState{..} ->
        wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle
  where
    HasFS{..} = _dbHasFS

isOpenDBImpl :: IOLike m
             => VolatileDBEnv m blockId
             -> m Bool
isOpenDBImpl VolatileDBEnv{..} = do
    mSt <- readMVar _dbInternalState
    return $ volatileDbIsOpen mSt

-- | Property: @'closeDB' >> 'reOpenDB'@  should be a no-op. This is true
-- because 'reOpenDB' will always append to the last created file.
reOpenDBImpl :: ( HasCallStack
                , IOLike m
                , Ord      blockId
                , Typeable blockId
                , Show     blockId
                )
             => VolatileDBEnv m blockId
             -> m ()
reOpenDBImpl VolatileDBEnv{..} =
    modifyMVar _dbInternalState $ \case
      VolatileDbOpen st -> do
        traceWith _tracer DBAlreadyOpen
        return (VolatileDbOpen st, ())
      VolatileDbClosed -> do
        st <- mkInternalStateDB
          _dbHasFS _dbErr _parser _tracer _maxBlocksPerFile
        return (VolatileDbOpen st, ())

getBlockComponentImpl
  :: forall m blockId b. (IOLike m, Ord blockId, HasCallStack)
  => VolatileDBEnv m blockId
  -> BlockComponent (VolatileDB blockId m) b
  -> blockId
  -> m (Maybe b)
getBlockComponentImpl env@VolatileDBEnv{..} blockComponent blockId =
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
      case Map.lookup blockId _currentRevMap of
        Nothing                -> return (st, Nothing)
        Just internalBlockInfo -> ((st, ) . Just) <$>
          getBlockComponent hasFS internalBlockInfo blockComponent
  where
    getBlockComponent
      :: forall b' h.
         HasFS m h
      -> InternalBlockInfo blockId
      -> BlockComponent (VolatileDB blockId m) b'
      -> m b'
    getBlockComponent hasFS ib = \case
        GetHash         -> return blockId
        GetSlot         -> return bslot
        GetIsEBB        -> return bisEBB
        GetBlockSize    -> return $ fromIntegral $ unBlockSize ibBlockSize
        GetHeaderSize   -> return bheaderSize
        GetPure a       -> return a
        GetApply f bc   ->
          getBlockComponent hasFS ib f <*> getBlockComponent hasFS ib bc
        GetBlock        -> return ()
        GetRawBlock     -> withFile hasFS ibFile ReadMode $ \hndl -> do
          let size   = unBlockSize ibBlockSize
              offset = ibBlockOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetHeader       -> return ()
        GetRawHeader    -> withFile hasFS ibFile ReadMode $ \hndl -> do
          let size   = fromIntegral bheaderSize
              offset = ibBlockOffset + fromIntegral bheaderOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
      where
        InternalBlockInfo { ibBlockInfo = BlockInfo {..}, .. } = ib

-- | This function follows the approach:
-- (1) hPut bytes to the file
-- (2) if full hClose the write file
-- (3)         hOpen a new write file
-- (4) update the Internal State.
--
-- If there is an error after (1) or after (2) we should make sure that when
-- we reopen a db from scratch, it can successfully recover, even if it does
-- not find an empty file to write and all other files are full.
--
-- We should also make sure that the db can recover if we get an
-- exception/error at any moment and that we are left with an empty Internal
-- State.
--
-- We should be careful about not leaking open fds when we open a new file,
-- since this can affect garbage collection of files.
putBlockImpl :: forall m blockId. (IOLike m, Ord blockId)
             => VolatileDBEnv m blockId
             -> BlockInfo blockId
             -> BS.Builder
             -> m ()
putBlockImpl env@VolatileDBEnv{..} blockInfo@BlockInfo { bbid, bslot, bpreBid } builder =
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
      if Map.member bbid _currentRevMap then do
        traceWith _tracer $ BlockAlreadyHere bbid
        return (st, ()) -- putting an existing block is a no-op.
      else do
        bytesWritten <- hPut hasFS _currentWriteHandle builder
        updateStateAfterWrite hasFS st bytesWritten
  where
    updateStateAfterWrite :: forall h.
                             HasFS m h
                          -> InternalState blockId h
                          -> Word64
                          -> m (InternalState blockId h, ())
    updateStateAfterWrite hasFS@HasFS{..} st@InternalState{..} bytesWritten =
        if FileInfo.isFull _maxBlocksPerFile fileInfo'
        then (,()) <$> nextFile hasFS _dbErr env st'
        else return (st', ())
      where
        fileInfo = fromMaybe
            (error $ "VolatileDB invariant violation:"
                    ++ "Current write file not found in Index.")
            (Index.lookup _currentWriteId _currentMap)
        fileBlockInfo = FileInfo.mkFileBlockInfo (BlockSize bytesWritten) bbid
        fileInfo' = FileInfo.addBlock bslot _currentWriteOffset fileBlockInfo fileInfo
        currentMap' = Index.insert _currentWriteId fileInfo' _currentMap
        internalBlockInfo' = InternalBlockInfo {
            ibFile         = _currentWritePath
          , ibBlockOffset  = _currentWriteOffset
          , ibBlockSize    = BlockSize bytesWritten
          , ibBlockInfo    = blockInfo
          }
        currentRevMap' = Map.insert bbid internalBlockInfo' _currentRevMap
        st' = st {
            _currentWriteOffset = _currentWriteOffset + bytesWritten
          , _currentMap         = currentMap'
          , _currentRevMap      = currentRevMap'
          , _currentSuccMap     = insertMapSet _currentSuccMap (bbid, bpreBid)
          , _currentMaxSlotNo   = _currentMaxSlotNo `max` MaxSlotNo bslot
          }

-- | The approach we follow here is to try to garbage collect each file.
-- For each file we update the fs and then we update the Internal State.
-- If some fs update fails, we are left with an empty Internal State and a
-- subset of the deleted files in fs. Any unexpected failure (power loss,
-- other exceptions) has the same results, since the Internal State will
-- be empty on re-opening. This is ok only if any fs updates leave the fs
-- in a consistent state every moment.
--
-- This approach works since we always close the Database in case of errors,
-- but we should rethink it if this changes in the future.
garbageCollectImpl :: forall m blockId. (IOLike m, Ord blockId)
                   => VolatileDBEnv m blockId
                   -> SlotNo
                   -> m ()
garbageCollectImpl env@VolatileDBEnv{..} slot =
    modifyState env $ \hasFS st -> do
      st' <- foldM (tryCollectFile hasFS env slot) st
              (sortOn fst $ Index.toList (_currentMap st))
      -- Recompute the 'MaxSlotNo' based on the files left in the VolatileDB.
      -- This value can never go down, except to 'NoMaxSlotNo' (when we GC
      -- everything), because a GC can only delete blocks < a slot.
      let st'' = st' {
              _currentMaxSlotNo = FileInfo.maxSlotInFiles
                (Index.elems (_currentMap st'))
            }
      return (st'', ())

-- | For the given file, we garbage collect it if possible and return the
-- updated 'InternalState'.
--
-- NOTE: the current file is never garbage collected.
--
-- Important to note here is that, every call should leave the file system in
-- a consistent state, without depending on other calls. We achieve this by
-- only needed a single system call: 'removeFile'.
--
-- NOTE: the returned 'InternalState' is inconsistent in the follow respect:
-- the cached '_currentMaxSlotNo' hasn't been updated yet.
--
-- This may throw an FsError.
tryCollectFile :: forall m h blockId
               .  (MonadThrow m, Ord blockId)
               => HasFS m h
               -> VolatileDBEnv m blockId
               -> SlotNo
               -> InternalState blockId h
               -> (FileId, FileInfo blockId)
               -> m (InternalState blockId h)
tryCollectFile hasFS VolatileDBEnv{..} slot st (fileId, fileInfo)
    | FileInfo.canGC fileInfo slot && not isCurrent
      -- We don't GC the current file. This is unlikely to happen in practice
      -- anyway, and it makes things simpler.
    = do
      removeFile hasFS $ filePath fileId
      return st {
          _currentMap     = Index.delete fileId _currentMap
        , _currentRevMap  = currentRevMap'
        , _currentSuccMap = succMap'
        }

    | otherwise
    = return st
  where
    InternalState{..} = st
    isCurrent         = fileId == _currentWriteId
    bids              = FileInfo.blockIds fileInfo
    currentRevMap'    = Map.withoutKeys _currentRevMap (Set.fromList bids)
    deletedPairs      =
        mapMaybe (\b -> (b,) . bpreBid . ibBlockInfo <$> Map.lookup b _currentRevMap) bids
    succMap'          = foldl' deleteMapSet _currentSuccMap deletedPairs

getSuccessorsImpl :: forall m blockId. (IOLike m, Ord blockId)
                  => VolatileDBEnv m blockId
                  -> STM m (WithOrigin blockId -> Set blockId)
getSuccessorsImpl = getterSTM $ \st blockId ->
    fromMaybe Set.empty (Map.lookup blockId (_currentSuccMap st))

getBlockInfoImpl :: forall m blockId. (IOLike m, Ord blockId)
                 => VolatileDBEnv m blockId
                 -> STM m (blockId -> Maybe (BlockInfo blockId))
getBlockInfoImpl = getterSTM $ \st blockId ->
    ibBlockInfo <$> Map.lookup blockId (_currentRevMap st)

getMaxSlotNoImpl :: forall m blockId. IOLike m
                 => VolatileDBEnv m blockId
                 -> STM m MaxSlotNo
getMaxSlotNoImpl = getterSTM _currentMaxSlotNo

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- | Creates a new file and updates the 'InternalState' accordingly.
-- This may throw an FsError.
nextFile :: forall h m blockId. IOLike m
         => HasFS m h
         -> ErrorHandling VolatileDBError m
         -> VolatileDBEnv m blockId
         -> InternalState blockId h
         -> m (InternalState blockId h)
nextFile HasFS{..} _err VolatileDBEnv{..} st@InternalState{..} = do
    hClose _currentWriteHandle
    hndl <- hOpen file (AppendMode MustBeNew)
    return st {
        _currentWriteHandle = hndl
      , _currentWritePath   = file
      , _currentWriteId     = currentWriteId'
      , _currentWriteOffset = 0
      , _currentMap         = Index.insert currentWriteId' FileInfo.empty
                                _currentMap
      }
  where
    currentWriteId' = _currentWriteId + 1
    file = filePath currentWriteId'

mkInternalStateDB :: forall m blockId e h.
                     ( HasCallStack
                     , MonadThrow m
                     , MonadCatch m
                     , Ord      blockId
                     , Typeable blockId
                     , Show     blockId
                     )
                  => HasFS m h
                  -> ErrorHandling VolatileDBError m
                  -> Parser e m blockId
                  -> Tracer m (TraceEvent e blockId)
                  -> BlocksPerFile
                  -> m (InternalState blockId h)
mkInternalStateDB hasFS@HasFS{..} err parser tracer maxBlocksPerFile =
    wrapFsError hasFsErr err $ do
      createDirectoryIfMissing True dbDir
      allFiles <- map toFsPath . Set.toList <$> listDirectory dbDir
      filesWithIds <- logInvalidFiles $ parseAllFds allFiles
      mkInternalState hasFS err parser tracer maxBlocksPerFile filesWithIds
  where
    -- | Logs about any invalid 'FsPath' and returns the valid ones.
    logInvalidFiles :: ([(FileId, FsPath)], [FsPath]) -> m [(FileId, FsPath)]
    logInvalidFiles (valid, invalid) = do
      unless (null invalid) $
        traceWith tracer $ InvalidFileNames invalid
      return valid

    dbDir = mkFsPath []

    toFsPath :: String -> FsPath
    toFsPath file = mkFsPath [file]

-- | Short-hand for all three index types
type Indices blockId =
  ( Index           blockId
  , ReverseIndex    blockId
  , SuccessorsIndex blockId
  )

-- | Makes the 'InternalState' by parsing all files.
--
-- It may create a new file to append new blocks to or use an existing one.
mkInternalState
  :: forall blockId m h e. (
       HasCallStack
     , MonadCatch m
     , Ord      blockId
     , Typeable blockId
     , Show     blockId
     )
  => HasFS m h
  -> ErrorHandling VolatileDBError m
  -> Parser e m blockId
  -> Tracer m (TraceEvent e blockId)
  -> BlocksPerFile
  -> [(FileId, FsPath)]
  -> m (InternalState blockId h)
mkInternalState hasFS err parser tracer maxBlocksPerFile files =
    wrapFsError (hasFsErr hasFS) err $ do
      (currentMap', currentRevMap', currentSuccMap') <-
        foldM validateFile (Index.empty, Map.empty, Map.empty) files

      let (currentWriteId, currentMap'') = case Index.lastFile currentMap' of
            -- The DB is empty. Create a new file with 'FileId' 0
            Nothing
              -> (0, Index.insert 0 FileInfo.empty currentMap')
            Just (lastWriteId, lastFileInfo)
              | FileInfo.isFull maxBlocksPerFile lastFileInfo
              , let nextWriteId = lastWriteId + 1
                -- If the last file is full, we need to create a new one
              -> (nextWriteId, Index.insert nextWriteId FileInfo.empty currentMap')
              | otherwise
                -- If the last file is not full, then use that one
              -> (lastWriteId, currentMap')

      let currentWritePath = filePath currentWriteId

      currentWriteHandle <- hOpen hasFS currentWritePath (AppendMode AllowExisting)
      -- If 'hGetSize' fails, we should close the opened handle that didn't
      -- make it into the state, otherwise we'd leak it.
      currentWriteOffset <-
        EH.onException (hasFsErr hasFS)
          (hGetSize hasFS currentWriteHandle)
          (hClose   hasFS currentWriteHandle)

      return InternalState {
          _currentWriteHandle = currentWriteHandle
        , _currentWritePath   = currentWritePath
        , _currentWriteId     = currentWriteId
        , _currentWriteOffset = currentWriteOffset
        , _currentMap         = currentMap''
        , _currentRevMap      = currentRevMap'
        , _currentSuccMap     = currentSuccMap'
        , _currentMaxSlotNo   = FileInfo.maxSlotInFiles
                                  (Index.elems currentMap')
        }
  where
    validateFile :: Indices blockId -> (FileId, FsPath) -> m (Indices blockId)
    validateFile (currentMap, currentRevMap, currentSuccMap) (fd, file) = do
      (parsedBlocks, mErr) <- parse parser file
      whenJust mErr $ \(e, offset) ->
        truncateError file e offset

      let (currentRevMap', acceptedBlocks, mErr') =
            addToReverseIndex file currentRevMap parsedBlocks
      -- We can find duplicate blocks when merging the parsed blocks with the
      -- 'ReverseIndex', so we might have to truncate at this point too.
      whenJust mErr' $ \(e, offset) ->
        truncateError file e offset

      let fileInfo        = FileInfo.fromParsedInfo acceptedBlocks
          currentMap'     = Index.insert fd fileInfo currentMap
          currentSuccMap' = foldl'
            (\succMap (_, (_, blockInfo)) ->
              insertMapSet succMap (bbid blockInfo, bpreBid blockInfo))
            currentSuccMap
            acceptedBlocks

      return (currentMap', currentRevMap', currentSuccMap')

    truncateError
      :: FsPath
      -> ParserError blockId e
      -> BlockOffset
      -> m ()
    truncateError file e offset = do
      traceWith tracer $ Truncate e file offset
      -- The handle of the parser is closed at this point. We need
      -- to reopen the file in 'AppendMode' now (parser opens with
      -- 'ReadMode').
      --
      -- Note that no file is open at this point, so we can safely
      -- open with 'AppendMode' any file, without the fear of opening
      -- multiple concurrent writers, which is not allowed, or concurrent
      -- read with truncate.
      withFile hasFS file (AppendMode AllowExisting) $ \hndl ->
        hTruncate hasFS hndl offset

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyState :: forall blockId m r. (HasCallStack, IOLike m)
            => VolatileDBEnv m blockId
            -> (forall h
               .  HasFS m h
               -> InternalState blockId h
               -> m (InternalState blockId h, r)
               )
            -> m r
modifyState VolatileDBEnv{_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (tryVolDB hasFsErr _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (_, r) -> return r
  where
    ErrorHandling{..} = _dbErr
    HasFS{..}         = hasFS

    open :: m (OpenOrClosed blockId h)
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    open = uninterruptibleMask_ $ takeMVar _dbInternalState

    close
      :: OpenOrClosed blockId h
      -> ExitCase (Either VolatileDBError (InternalState blockId h, r))
      -> m ()
    close mst ec = do
        -- It is crucial to replace the TMVar.
        putMVar _dbInternalState mst'
        followUp
      where
        (mst', followUp) = case ec of
          -- If we were interrupted, restore the original state.
          ExitCaseAbort                         -> (mst, return ())
          ExitCaseException _ex                 -> (mst, return ())
          -- In case of success, update to the newest state.
          ExitCaseSuccess (Right (newState, _)) ->
            (VolatileDbOpen newState, return ())
          -- In case of an error (not an exception), close the DB for safety.
          ExitCaseSuccess (Left _)              ->
            (VolatileDbClosed, closeOpenHandle mst)

    mutation :: OpenOrClosed blockId h
             -> m (InternalState blockId h, r)
    mutation VolatileDbClosed          = throwError $ UserError ClosedDBError
    mutation (VolatileDbOpen oldState) = action hasFS oldState

    -- TODO what if this fails?
    closeOpenHandle :: OpenOrClosed blockId h -> m ()
    closeOpenHandle VolatileDbClosed                    = return ()
    closeOpenHandle (VolatileDbOpen InternalState {..}) =
      wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle

-- | Gets part of the 'InternalState' in 'STM'.
getterSTM :: forall m blockId a. IOLike m
          => (forall h. InternalState blockId h -> a)
          -> VolatileDBEnv m blockId
          -> STM m a
getterSTM fromSt VolatileDBEnv{..} = do
    mSt <- readMVarSTM _dbInternalState
    case mSt of
      VolatileDbClosed  -> EH.throwError' _dbErrSTM $ UserError ClosedDBError
      VolatileDbOpen st -> return $ fromSt st

-- | For each block found in a parsed file, we insert its 'InternalBlockInfo'
-- in the 'ReverseIndex'.
--
-- If a block is already present in the 'ReverseIndex' or occurs twice in the
-- same file, we stop with an error.
--
-- We return:
--
-- * A 'ReverseIndex' updated with the valid blocks
-- * A list of the valid blocks in the parsed file. This will be a prefix of
--   the given list, or most often, the original input list.
-- * In case of an error, the error and the offset to truncate to.
addToReverseIndex
  :: forall blockId e. (
       Ord      blockId
     , Typeable blockId
     , Show     blockId
     )
  => FsPath
  -> ReverseIndex blockId
  -> ParsedInfo blockId
  -> ( ReverseIndex blockId
     , ParsedInfo blockId
     , Maybe (ParserError blockId e, BlockOffset)
     )
addToReverseIndex file = \revMap -> go revMap []
  where
    go :: ReverseIndex blockId
       -> ParsedInfo blockId -- accumulator of the accepted blocks.
       -> ParsedInfo blockId
       -> ( ReverseIndex blockId
          , ParsedInfo blockId
          , Maybe (ParserError blockId e, BlockOffset)
          )
    go revMap acc = \case
      []               -> (revMap, reverse acc, Nothing)
      parsedBlock:rest -> case insertNew bbid internalBlockInfo revMap of
          Right revMap' -> go revMap' (parsedBlock:acc) rest
          Left InternalBlockInfo { ibFile = alreadyExistsHere } ->
              ( revMap
              , reverse acc
              , Just (DuplicatedBlock bbid alreadyExistsHere file, offset)
              )
        where
          (offset, (size, blockInfo@BlockInfo { bbid })) = parsedBlock
          internalBlockInfo = InternalBlockInfo {
              ibFile         = file
            , ibBlockOffset  = offset
            , ibBlockSize    = size
            , ibBlockInfo    = blockInfo
            }

    -- | Insert the value at the key returning the updated map, unless there
    -- already is a key at the same location, in which case we return the
    -- original value.
    --
    -- Should be more efficient than the combination of 'Map.lookup' and
    -- 'Map.insert'.
    insertNew :: forall k a. Ord k => k -> a -> Map k a -> Either a (Map k a)
    insertNew k a m =
      case Map.insertLookupWithKey (\_k new _old -> new) k a m of
        (Nothing, m') -> Right m'
        (Just a', _)  -> Left a'
