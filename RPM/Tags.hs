-- Copyright (C) 2016 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}

module RPM.Tags(Tag(..),
                Null(..),
                findByteStringTag,
                findTag,
                findStringTag,
                findStringListTag,
                mkTag,
                tagValue)
 where

import           Data.Bits((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Data(Data, cast, gmapQi, showConstr, toConstr)
import           Data.List(find)
import           Data.Maybe(fromMaybe)
import           Data.Typeable(Typeable)
import           Data.Word
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(text)

import RPM.Internal.Numbers

{-# ANN module "HLint: ignore Use camelCase" #-}

data Tag = DEPRECATED                   Tag
         | INTERNAL                     Tag
         | OBSOLETE                     Tag
         | UNIMPLEMENTED                Tag
         | UNUSED                       Tag

         | HeaderImage                  Null Word32 Word32
         | HeaderSignatures             Null Word32 Word32
         | HeaderImmutable              Null Word32 Word32
         | HeaderRegions                Null Word32 Word32
         | HeaderI18NTable              [String] Word32 Word32

         | SigBase                      Null Word32 Word32
         | SigSize                      Word32 Word32 Word32
         | SigLEMD5_1                   Null Word32 Word32
         | SigPGP                       BS.ByteString Word32 Word32
         | SigLEMD5_2                   Null Word32 Word32
         | SigMD5                       BS.ByteString Word32 Word32
         | SigGPG                       BS.ByteString Word32 Word32
         | SigPGP5                      Null Word32 Word32
         | SigBadSHA1_1                 Null Word32 Word32
         | SigBadSHA1_2                 Null Word32 Word32
         | PubKeys                      [String] Word32 Word32
         | DSAHeader                    BS.ByteString Word32 Word32
         | RSAHeader                    BS.ByteString Word32 Word32
         | SHA1Header                   String Word32 Word32
         | LongSigSize                  Word64 Word32 Word32
         | LongArchiveSize              Word64 Word32 Word32

         | Name                         String Word32 Word32
         | Version                      String Word32 Word32
         | Release                      String Word32 Word32
         | Epoch                        Word32 Word32 Word32
         | Summary                      BS.ByteString Word32 Word32
         | Description                  BS.ByteString Word32 Word32
         | BuildTime                    Word32 Word32 Word32
         | BuildHost                    String Word32 Word32
         | InstallTime                  Word32 Word32 Word32
         | Size                         Word32 Word32 Word32
         | Distribution                 String Word32 Word32
         | Vendor                       String Word32 Word32
         | GIF                          BS.ByteString Word32 Word32
         | XPM                          BS.ByteString Word32 Word32
         | License                      String Word32 Word32
         | Packager                     String Word32 Word32
         | Group                        BS.ByteString Word32 Word32
         | ChangeLog                    [String] Word32 Word32
         | Source                       [String] Word32 Word32
         | Patch                        [String] Word32 Word32
         | URL                          String Word32 Word32
         | OS                           String Word32 Word32
         | Arch                         String Word32 Word32
         | PreIn                        String Word32 Word32
         | PostIn                       String Word32 Word32
         | PreUn                        String Word32 Word32
         | PostUn                       String Word32 Word32
         | OldFileNames                 [String] Word32 Word32
         | FileSizes                    [Word32] Word32 Word32
         | FileStates                   [Char] Word32 Word32
         | FileModes                    [Word16] Word32 Word32
         | FileUIDs                     [Word32] Word32 Word32
         | FileGIDs                     [Word32] Word32 Word32
         | FileRDevs                    [Word16] Word32 Word32
         | FileMTimes                   [Word32] Word32 Word32
         | FileMD5s                     [String] Word32 Word32
         | FileLinkTos                  [String] Word32 Word32
         | FileFlags                    Word32 Word32 Word32
         | Root                         Null Word32 Word32
         | FileUserName                 [String] Word32 Word32
         | FileGroupName                [String] Word32 Word32
         | Exclude                      Null Word32 Word32
         | Exclusive                    Null Word32 Word32
         | Icon                         BS.ByteString Word32 Word32
         | SourceRPM                    String Word32 Word32
         | FileVerifyFlags              [Word32] Word32 Word32
         | ArchiveSize                  Word32 Word32 Word32
         | ProvideName                  [String] Word32 Word32
         | RequireFlags                 [Word32] Word32 Word32
         | RequireName                  [String] Word32 Word32
         | RequireVersion               [String] Word32 Word32
         | NoSource                     [Word32] Word32 Word32
         | NoPatch                      [Word32] Word32 Word32
         | ConflictFlags                Word32 Word32 Word32
         | ConflictName                 [String] Word32 Word32
         | ConflictVersion              [String] Word32 Word32
         | DefaultPrefix                String Word32 Word32
         | BuildRoot                    String Word32 Word32
         | InstallPrefix                String Word32 Word32
         | ExcludeArch                  [String] Word32 Word32
         | ExcludeOS                    [String] Word32 Word32
         | ExclusiveArch                [String] Word32 Word32
         | ExclusiveOS                  [String] Word32 Word32
         | AutoReqProv                  String Word32 Word32
         | RPMVersion                   String Word32 Word32
         | TriggerScripts               [String] Word32 Word32
         | TriggerName                  [String] Word32 Word32
         | TriggerVersion               [String] Word32 Word32
         | TriggerFlags                 [Word32] Word32 Word32
         | TriggerIndex                 [Word32] Word32 Word32
         | VerifyScript                 String Word32 Word32
         | ChangeLogTime                [Word32] Word32 Word32
         | ChangeLogName                [String] Word32 Word32
         | ChangeLogText                [String] Word32 Word32
         | BrokenMD5                    Null Word32 Word32
         | PreReq                       Null Word32 Word32
         | PreInProg                    [String] Word32 Word32
         | PostInProg                   [String] Word32 Word32
         | PreUnProg                    [String] Word32 Word32
         | PostUnProg                   [String] Word32 Word32
         | BuildArchs                   [String] Word32 Word32
         | ObsoleteName                 [String] Word32 Word32
         | VerifyScriptProg             [String] Word32 Word32
         | TriggerScriptProg            [String] Word32 Word32
         | DocDir                       Null Word32 Word32
         | Cookie                       String Word32 Word32
         | FileDevices                  [Word32] Word32 Word32
         | FileINodes                   [Word32] Word32 Word32
         | FileLangs                    [String] Word32 Word32
         | Prefixes                     [String] Word32 Word32
         | InstPrefixes                 [String] Word32 Word32
         | TriggerIn                    Null Word32 Word32
         | TriggerUn                    Null Word32 Word32
         | TriggerPostUn                Null Word32 Word32
         | AutoReq                      Null Word32 Word32
         | AutoProv                     Null Word32 Word32
         | Capability                   Word32 Word32 Word32
         | SourcePackage                Word32 Word32 Word32
         | OldOrigFileNames             Null Word32 Word32
         | BuildPreReq                  Null Word32 Word32
         | BuildRequires                Null Word32 Word32
         | BuildConflicts               Null Word32 Word32
         | BuildMacros                  Null Word32 Word32
         | ProvideFlags                 [Word32] Word32 Word32
         | ProvideVersion               [String] Word32 Word32
         | ObsoleteFlags                [Word32] Word32 Word32
         | ObsoleteVersion              [String] Word32 Word32
         | DirIndexes                   [Word32] Word32 Word32
         | BaseNames                    [String] Word32 Word32
         | DirNames                     [String] Word32 Word32
         | OrigDirIndexes               [Word32] Word32 Word32
         | OrigBaseNames                [String] Word32 Word32
         | OrigDirNames                 [String] Word32 Word32
         | OptFlags                     String Word32 Word32
         | DistURL                      String Word32 Word32
         | PayloadFormat                String Word32 Word32
         | PayloadCompressor            String Word32 Word32
         | PayloadFlags                 String Word32 Word32
         | InstallColor                 Word32 Word32 Word32
         | InstallTID                   Word32 Word32 Word32
         | RemoveTID                    Word32 Word32 Word32
         | SHA1RHN                      Null Word32 Word32
         | RHNPlatform                  String Word32 Word32
         | Platform                     String Word32 Word32
         | PatchesName                  [String] Word32 Word32
         | PatchesFlags                 [Word32] Word32 Word32
         | PatchesVersion               [String] Word32 Word32
         | CacheCTime                   Word32 Word32 Word32
         | CachePkgPath                 String Word32 Word32
         | CachePkgSize                 Word32 Word32 Word32
         | CachePkgMTime                Word32 Word32 Word32
         | FileColors                   [Word32] Word32 Word32
         | FileClass                    [Word32] Word32 Word32
         | ClassDict                    [String] Word32 Word32
         | FileDependsX                 [Word32] Word32 Word32
         | FileDependsN                 [Word32] Word32 Word32
         | DependsDict                  [(Word32, Word32)] Word32 Word32
         | SourcePkgID                  BS.ByteString Word32 Word32
         | FileContexts                 [String] Word32 Word32
         | FSContexts                   [String] Word32 Word32
         | ReContexts                   [String] Word32 Word32
         | Policies                     [String] Word32 Word32
         | PreTrans                     String Word32 Word32
         | PostTrans                    String Word32 Word32
         | PreTransProg                 [String] Word32 Word32
         | PostTransProg                [String] Word32 Word32
         | DistTag                      String Word32 Word32
         | OldSuggestsName              [String] Word32 Word32
         | OldSuggestsVersion           [String] Word32 Word32
         | OldSuggestsFlags             [Word32] Word32 Word32
         | OldEnhancesName              [String] Word32 Word32
         | OldEnhancesVersion           [String] Word32 Word32
         | OldEnhancesFlags             [Word32] Word32 Word32
         | Priority                     [Word32] Word32 Word32
         | CVSID                        String Word32 Word32
         | BLinkPkgID                   [String] Word32 Word32
         | BLinkHdrID                   [String] Word32 Word32
         | BLinkNEVRA                   [String] Word32 Word32
         | FLinkPkgID                   [String] Word32 Word32
         | FLinkHdrID                   [String] Word32 Word32
         | FLinkNEVRA                   [String] Word32 Word32
         | PackageOrigin                String Word32 Word32
         | TriggerPreIn                 Null Word32 Word32
         | BuildSuggests                Null Word32 Word32
         | BuildEnhances                Null Word32 Word32
         | ScriptStates                 [Word32] Word32 Word32
         | ScriptMetrics                [Word32] Word32 Word32
         | BuildCPUClock                Word32 Word32 Word32
         | FileDigestAlgos              [Word32] Word32 Word32
         | Variants                     [String] Word32 Word32
         | XMajor                       Word32 Word32 Word32
         | XMinor                       Word32 Word32 Word32
         | RepoTag                      String Word32 Word32
         | Keywords                     [String] Word32 Word32
         | BuildPlatforms               [String] Word32 Word32
         | PackageColor                 Word32 Word32 Word32
         | PackagePrefColor             Word32 Word32 Word32
         | XattrsDict                   [String] Word32 Word32
         | FileXattrsx                  [Word32] Word32 Word32
         | DepAttrsDict                 [String] Word32 Word32
         | ConflictAttrsx               [Word32] Word32 Word32
         | ObsoleteAttrsx               [Word32] Word32 Word32
         | ProvideAttrsx                [Word32] Word32 Word32
         | RequireAttrsx                [Word32] Word32 Word32
         | BuildProvides                Null Word32 Word32
         | BuildObsoletes               Null Word32 Word32
         | DBInstance                   Word32 Word32 Word32
         | NVRA                         String Word32 Word32

         | FileNames                    [String] Word32 Word32
         | FileProvide                  [String] Word32 Word32
         | FileRequire                  [String] Word32 Word32
         | FSNames                      [String] Word32 Word32
         | FSSizes                      [Word64] Word32 Word32
         | TriggerConds                 [String] Word32 Word32
         | TriggerType                  [String] Word32 Word32
         | OrigFileNames                [String] Word32 Word32
         | LongFileSizes                [Word64] Word32 Word32
         | LongSize                     Word64 Word32 Word32
         | FileCaps                     [String] Word32 Word32
         | FileDigestAlgo               Word32 Word32 Word32
         | BugURL                       String Word32 Word32
         | EVR                          String Word32 Word32
         | NVR                          String Word32 Word32
         | NEVR                         String Word32 Word32
         | NEVRA                        String Word32 Word32
         | HeaderColor                  Word32 Word32 Word32
         | Verbose                      Word32 Word32 Word32
         | EpochNum                     Word32 Word32 Word32
         | PreInFlags                   Word32 Word32 Word32
         | PostInFlags                  Word32 Word32 Word32
         | PreUnFlags                   Word32 Word32 Word32
         | PostUnFlags                  Word32 Word32 Word32
         | PreTransFlags                Word32 Word32 Word32
         | PostTransFlags               Word32 Word32 Word32
         | VerifyScriptFlags            Word32 Word32 Word32
         | TriggerScriptFlags           [Word32] Word32 Word32
         | Collections                  [String] Word32 Word32
         | PolicyNames                  [String] Word32 Word32
         | PolicyTypes                  [String] Word32 Word32
         | PolicyTypesIndexes           [Word32] Word32 Word32
         | PolicyFlags                  [Word32] Word32 Word32
         | PolicyVCS                    String Word32 Word32
         | OrderName                    [String] Word32 Word32
         | OrderVersion                 [String] Word32 Word32
         | OrderFlags                   [Word32] Word32 Word32
         | MSSFManifest                 [String] Word32 Word32
         | MSSFDomain                   [String] Word32 Word32
         | InstFileNames                [String] Word32 Word32
         | RequireNEVRs                 [String] Word32 Word32
         | ProvideNEVRs                 [String] Word32 Word32
         | ObsoleteNEVRs                [String] Word32 Word32
         | ConflictNEVRs                [String] Word32 Word32
         | FileNLinks                   [Word32] Word32 Word32
         | RecommendName                [String] Word32 Word32
         | RecommendVersion             [String] Word32 Word32
         | RecommendFlags               [Word32] Word32 Word32
         | SuggestName                  [String] Word32 Word32
         | SuggestVersion               [String] Word32 Word32
         | SuggestFlags                 [Word32] Word32 Word32
         | SupplementName               [String] Word32 Word32
         | SupplementVersion            [String] Word32 Word32
         | SupplementFlags              [Word32] Word32 Word32
         | EnhanceName                  [String] Word32 Word32
         | EnhanceVersion               [String] Word32 Word32
         | EnhanceFlags                 [Word32] Word32 Word32
         | RecommendNEVRs               [String] Word32 Word32
         | SuggestNEVRs                 [String] Word32 Word32
         | SupplementNEVRs              [String] Word32 Word32
         | EnhanceNEVRs                 [String] Word32 Word32
         | Encoding                     String Word32 Word32
         | FileTriggerIn                Null Word32 Word32
         | FileTriggerUn                Null Word32 Word32
         | FileTriggerPostUn            Null Word32 Word32
         | FileTriggerScripts           [String] Word32 Word32
         | FileTriggerScriptProg        [String] Word32 Word32
         | FileTriggerScriptFlags       [Word32] Word32 Word32
         | FileTriggerName              [String] Word32 Word32
         | FileTriggerIndex             [Word32] Word32 Word32
         | FileTriggerVersion           [String] Word32 Word32
         | FileTriggerFlags             [Word32] Word32 Word32
         | TransFileTriggerIn           Null Word32 Word32
         | TransFileTriggerUn           Null Word32 Word32
         | TransFileTriggerPostUn       Null Word32 Word32
         | TransFileTriggerScripts      [String] Word32 Word32
         | TransFileTriggerScriptProg   [String] Word32 Word32
         | TransFileTriggerScriptFlags  [Word32] Word32 Word32
         | TransFileTriggerName         [String] Word32 Word32
         | TransFileTriggerIndex        [Word32] Word32 Word32
         | TransFileTriggerVersion      [String] Word32 Word32
         | TransFileTriggerFlags        [Word32] Word32 Word32
         | RemovePathPostFixes          String Word32 Word32
         | FileTriggerPriorities        [Word32] Word32 Word32
         | TransFileTriggerPriorities   [Word32] Word32 Word32
         | FileTriggerConds             [String] Word32 Word32
         | FileTriggerType              [String] Word32 Word32
         | TransFileTriggerConds        [String] Word32 Word32
         | TransFileTriggerType         [String] Word32 Word32
         | FileSignatures               [String] Word32 Word32
         | FileSignatureLength          Word32 Word32 Word32
  deriving(Eq, Show, Data, Typeable)

instance Pretty Tag where
    -- Drop the last two Word32s from the pretty printed version of a tag, since who cares?
    -- This is a lot quicker than having to provide a Pretty instance that takes every
    -- single Tag into account.
    pPrint t = text . unwords $ init . init $ words (show t)

data Null = Null
 deriving(Eq, Show, Data, Typeable)

mkTag :: BS.ByteString -> Int -> Word32 -> Word32 -> Word32 -> Maybe Tag
mkTag store tag ty offset count = case tag of
    61      -> maker mkNull          >>=            \v -> Just $ HeaderImage v offset count
    62      -> maker mkNull          >>=            \v -> Just $ HeaderSignatures v offset count
    63      -> maker mkNull          >>=            \v -> Just $ HeaderImmutable v offset count
    64      -> maker mkNull          >>=            \v -> Just $ HeaderRegions v offset count
    100     -> maker mkStringArray   >>=            \v -> Just $ HeaderI18NTable v offset count

    256     -> maker mkNull          >>=            \v -> Just $ SigBase v offset count
    257     -> maker mkWord32        >>= unlist >>= \v -> Just $ SigSize v offset count
    258     -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SigLEMD5_1 v offset count
    259     -> maker mkBinary        >>=            \v -> Just $ SigPGP v offset count
    260     -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SigLEMD5_2 v offset count
    261     -> maker mkBinary        >>=            \v -> Just $ SigMD5 v offset count
    262     -> maker mkBinary        >>=            \v -> Just $ SigGPG v offset count
    263     -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SigPGP5 v offset count
    264     -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SigBadSHA1_1 v offset count
    265     -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SigBadSHA1_2 v offset count
    266     -> maker mkStringArray   >>=            \v -> Just $ PubKeys v offset count
    267     -> maker mkBinary        >>=            \v -> Just $ DSAHeader v offset count
    268     -> maker mkBinary        >>=            \v -> Just $ RSAHeader v offset count
    269     -> maker mkString        >>=            \v -> Just $ SHA1Header v offset count
    270     -> maker mkWord64        >>= unlist >>= \v -> Just $ LongSigSize v offset count
    271     -> maker mkWord64        >>= unlist >>= \v -> Just $ LongArchiveSize v offset count

    1000    -> maker mkString        >>=            \v -> Just $ Name v offset count
    1001    -> maker mkString        >>=            \v -> Just $ Version v offset count
    1002    -> maker mkString        >>=            \v -> Just $ Release v offset count
    1003    -> maker mkWord32        >>= unlist >>= \v -> Just $ Epoch v offset count
    1004    -> maker mkI18NString    >>=            \v -> Just $ Summary v offset count
    1005    -> maker mkI18NString    >>=            \v -> Just $ Description v offset count
    1006    -> maker mkWord32        >>= unlist >>= \v -> Just $ BuildTime v offset count
    1007    -> maker mkString        >>=            \v -> Just $ BuildHost v offset count
    1008    -> maker mkWord32        >>= unlist >>= \v -> Just $ InstallTime v offset count
    1009    -> maker mkWord32        >>= unlist >>= \v -> Just $ Size v offset count
    1010    -> maker mkString        >>=            \v -> Just $ Distribution v offset count
    1011    -> maker mkString        >>=            \v -> Just $ Vendor v offset count
    1012    -> maker mkBinary        >>=            \v -> Just $ GIF v offset count
    1013    -> maker mkBinary        >>=            \v -> Just $ XPM v offset count
    1014    -> maker mkString        >>=            \v -> Just $ License v offset count
    1015    -> maker mkString        >>=            \v -> Just $ Packager v offset count
    1016    -> maker mkI18NString    >>=            \v -> Just $ Group v offset count
    1017    -> maker mkStringArray   >>=            \v -> Just $ INTERNAL $ ChangeLog v offset count
    1018    -> maker mkStringArray   >>=            \v -> Just $ Source v offset count
    1019    -> maker mkStringArray   >>=            \v -> Just $ Patch v offset count
    1020    -> maker mkString        >>=            \v -> Just $ URL v offset count
    1021    -> maker mkString        >>=            \v -> Just $ OS v offset count
    1022    -> maker mkString        >>=            \v -> Just $ Arch v offset count
    1023    -> maker mkString        >>=            \v -> Just $ PreIn v offset count
    1024    -> maker mkString        >>=            \v -> Just $ PostIn v offset count
    1025    -> maker mkString        >>=            \v -> Just $ PreUn v offset count
    1026    -> maker mkString        >>=            \v -> Just $ PostUn v offset count
    1027    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ OldFileNames v offset count
    1028    -> maker mkWord32        >>=            \v -> Just $ FileSizes v offset count
    1029    -> maker mkChar          >>=            \v -> Just $ FileStates v offset count
    1030    -> maker mkWord16        >>=            \v -> Just $ FileModes v offset count
    1031    -> maker mkWord32        >>=            \v -> Just $ INTERNAL $ OBSOLETE $ FileUIDs v offset count
    1032    -> maker mkWord32        >>=            \v -> Just $ INTERNAL $ OBSOLETE $ FileGIDs v offset count
    1033    -> maker mkWord16        >>=            \v -> Just $ FileRDevs v offset count
    1034    -> maker mkWord32        >>=            \v -> Just $ FileMTimes v offset count
    1035    -> maker mkStringArray   >>=            \v -> Just $ FileMD5s v offset count
    1036    -> maker mkStringArray   >>=            \v -> Just $ FileLinkTos v offset count
    1037    -> maker mkWord32        >>= unlist >>= \v -> Just $ FileFlags v offset count
    1038    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ Root v offset count
    1039    -> maker mkStringArray   >>=            \v -> Just $ FileUserName v offset count
    1040    -> maker mkStringArray   >>=            \v -> Just $ FileGroupName v offset count
    1041    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ Exclude v offset count
    1042    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ Exclusive v offset count
    1043    -> maker mkBinary        >>=            \v -> Just $ Icon v offset count
    1044    -> maker mkString        >>=            \v -> Just $ SourceRPM v offset count
    1045    -> maker mkWord32        >>=            \v -> Just $ FileVerifyFlags v offset count
    1046    -> maker mkWord32        >>= unlist >>= \v -> Just $ ArchiveSize v offset count
    1047    -> maker mkStringArray   >>=            \v -> Just $ ProvideName v offset count
    1048    -> maker mkWord32        >>=            \v -> Just $ RequireFlags v offset count
    1049    -> maker mkStringArray   >>=            \v -> Just $ RequireName v offset count
    1050    -> maker mkStringArray   >>=            \v -> Just $ RequireVersion v offset count
    1051    -> maker mkWord32        >>=            \v -> Just $ NoSource v offset count
    1052    -> maker mkWord32        >>=            \v -> Just $ NoPatch v offset count
    1053    -> maker mkWord32        >>= unlist >>= \v -> Just $ ConflictFlags v offset count
    1054    -> maker mkStringArray   >>=            \v -> Just $ ConflictName v offset count
    1055    -> maker mkStringArray   >>=            \v -> Just $ ConflictVersion v offset count
    1056    -> maker mkString        >>=            \v -> Just $ INTERNAL $ DEPRECATED $ DefaultPrefix v offset count
    1057    -> maker mkString        >>=            \v -> Just $ INTERNAL $ OBSOLETE $ BuildRoot v offset count
    1058    -> maker mkString        >>=            \v -> Just $ INTERNAL $ DEPRECATED $ InstallPrefix v offset count
    1059    -> maker mkStringArray   >>=            \v -> Just $ ExcludeArch v offset count
    1060    -> maker mkStringArray   >>=            \v -> Just $ ExcludeOS v offset count
    1061    -> maker mkStringArray   >>=            \v -> Just $ ExclusiveArch v offset count
    1062    -> maker mkStringArray   >>=            \v -> Just $ ExclusiveOS v offset count
    1063    -> maker mkString        >>=            \v -> Just $ INTERNAL $ AutoReqProv v offset count
    1064    -> maker mkString        >>=            \v -> Just $ RPMVersion v offset count
    1065    -> maker mkStringArray   >>=            \v -> Just $ TriggerScripts v offset count
    1066    -> maker mkStringArray   >>=            \v -> Just $ TriggerName v offset count
    1067    -> maker mkStringArray   >>=            \v -> Just $ TriggerVersion v offset count
    1068    -> maker mkWord32        >>=            \v -> Just $ TriggerFlags v offset count
    1069    -> maker mkWord32        >>=            \v -> Just $ TriggerIndex v offset count
    1079    -> maker mkString        >>=            \v -> Just $ VerifyScript v offset count
    1080    -> maker mkWord32        >>=            \v -> Just $ ChangeLogTime v offset count
    1081    -> maker mkStringArray   >>=            \v -> Just $ ChangeLogName v offset count
    1082    -> maker mkStringArray   >>=            \v -> Just $ ChangeLogText v offset count
    1083    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ BrokenMD5 v offset count
    1084    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ PreReq v offset count
    1085    -> maker mkStringArray   >>=            \v -> Just $ PreInProg v offset count
    1086    -> maker mkStringArray   >>=            \v -> Just $ PostInProg v offset count
    1087    -> maker mkStringArray   >>=            \v -> Just $ PreUnProg v offset count
    1088    -> maker mkStringArray   >>=            \v -> Just $ PostUnProg v offset count
    1089    -> maker mkStringArray   >>=            \v -> Just $ BuildArchs v offset count
    1090    -> maker mkStringArray   >>=            \v -> Just $ ObsoleteName v offset count
    1091    -> maker mkStringArray   >>=            \v -> Just $ VerifyScriptProg v offset count
    1092    -> maker mkStringArray   >>=            \v -> Just $ TriggerScriptProg v offset count
    1093    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ DocDir v offset count
    1094    -> maker mkString        >>=            \v -> Just $ Cookie v offset count
    1095    -> maker mkWord32        >>=            \v -> Just $ FileDevices v offset count
    1096    -> maker mkWord32        >>=            \v -> Just $ FileINodes v offset count
    1097    -> maker mkStringArray   >>=            \v -> Just $ FileLangs v offset count
    1098    -> maker mkStringArray   >>=            \v -> Just $ Prefixes v offset count
    1099    -> maker mkStringArray   >>=            \v -> Just $ InstPrefixes v offset count
    1100    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TriggerIn v offset count
    1101    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TriggerUn v offset count
    1102    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TriggerPostUn v offset count
    1103    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ AutoReq v offset count
    1104    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ AutoProv v offset count
    1105    -> maker mkWord32        >>= unlist >>= \v -> Just $ INTERNAL $ OBSOLETE $ Capability v offset count
    1106    -> maker mkWord32        >>= unlist >>= \v -> Just $ SourcePackage v offset count
    1107    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ OldOrigFileNames v offset count
    1108    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ BuildPreReq v offset count
    1109    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ BuildRequires v offset count
    1110    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ BuildConflicts v offset count
    1111    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ UNUSED $ BuildMacros v offset count
    1112    -> maker mkWord32        >>=            \v -> Just $ ProvideFlags v offset count
    1113    -> maker mkStringArray   >>=            \v -> Just $ ProvideVersion v offset count
    1114    -> maker mkWord32        >>=            \v -> Just $ ObsoleteFlags v offset count
    1115    -> maker mkStringArray   >>=            \v -> Just $ ObsoleteVersion v offset count
    1116    -> maker mkWord32        >>=            \v -> Just $ DirIndexes v offset count
    1117    -> maker mkStringArray   >>=            \v -> Just $ BaseNames v offset count
    1118    -> maker mkStringArray   >>=            \v -> Just $ DirNames v offset count
    1119    -> maker mkWord32        >>=            \v -> Just $ OrigDirIndexes v offset count
    1120    -> maker mkStringArray   >>=            \v -> Just $ OrigBaseNames v offset count
    1121    -> maker mkStringArray   >>=            \v -> Just $ OrigDirNames v offset count
    1122    -> maker mkString        >>=            \v -> Just $ OptFlags v offset count
    1123    -> maker mkString        >>=            \v -> Just $ DistURL v offset count
    1124    -> maker mkString        >>=            \v -> Just $ PayloadFormat v offset count
    1125    -> maker mkString        >>=            \v -> Just $ PayloadCompressor v offset count
    1126    -> maker mkString        >>=            \v -> Just $ PayloadFlags v offset count
    1127    -> maker mkWord32        >>= unlist >>= \v -> Just $ InstallColor v offset count
    1128    -> maker mkWord32        >>= unlist >>= \v -> Just $ InstallTID v offset count
    1129    -> maker mkWord32        >>= unlist >>= \v -> Just $ RemoveTID v offset count
    1130    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ OBSOLETE $ SHA1RHN v offset count
    1131    -> maker mkString        >>=            \v -> Just $ INTERNAL $ OBSOLETE $ RHNPlatform v offset count
    1132    -> maker mkString        >>=            \v -> Just $ Platform v offset count
    1133    -> maker mkStringArray   >>=            \v -> Just $ DEPRECATED $ PatchesName v offset count
    1134    -> maker mkWord32        >>=            \v -> Just $ DEPRECATED $ PatchesFlags v offset count
    1135    -> maker mkStringArray   >>=            \v -> Just $ DEPRECATED $ PatchesVersion v offset count
    1136    -> maker mkWord32        >>= unlist >>= \v -> Just $ INTERNAL $ OBSOLETE $ CacheCTime v offset count
    1137    -> maker mkString        >>=            \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgPath v offset count
    1138    -> maker mkWord32        >>= unlist >>= \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgSize v offset count
    1139    -> maker mkWord32        >>= unlist >>= \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgMTime v offset count
    1140    -> maker mkWord32        >>=            \v -> Just $ FileColors v offset count
    1141    -> maker mkWord32        >>=            \v -> Just $ FileClass v offset count
    1142    -> maker mkStringArray   >>=            \v -> Just $ ClassDict v offset count
    1143    -> maker mkWord32        >>=            \v -> Just $ FileDependsX v offset count
    1144    -> maker mkWord32        >>=            \v -> Just $ FileDependsN v offset count
    1145    -> maker mkWord32        >>=            \v -> Just $ DependsDict (map (\x -> ((x `shiftR` 24) .&. 0xff, x .&. 0x00ffffff)) v) offset count
    1146    -> maker mkBinary        >>=            \v -> Just $ SourcePkgID v offset count
    1147    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ FileContexts v offset count
    1148    -> maker mkStringArray   >>=            \v -> Just $ FSContexts v offset count
    1149    -> maker mkStringArray   >>=            \v -> Just $ ReContexts v offset count
    1150    -> maker mkStringArray   >>=            \v -> Just $ Policies v offset count
    1151    -> maker mkString        >>=            \v -> Just $ PreTrans v offset count
    1152    -> maker mkString        >>=            \v -> Just $ PostTrans v offset count
    1153    -> maker mkStringArray   >>=            \v -> Just $ PreTransProg v offset count
    1154    -> maker mkStringArray   >>=            \v -> Just $ PostTransProg v offset count
    1155    -> maker mkString        >>=            \v -> Just $ DistTag v offset count
    1156    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ OldSuggestsName v offset count
    1157    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ OldSuggestsVersion v offset count
    1158    -> maker mkWord32        >>=            \v -> Just $ OBSOLETE $ OldSuggestsFlags v offset count
    1159    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ OldEnhancesName v offset count
    1160    -> maker mkStringArray   >>=            \v -> Just $ OBSOLETE $ OldEnhancesVersion v offset count
    1161    -> maker mkWord32        >>=            \v -> Just $ OBSOLETE $ OldEnhancesFlags v offset count
    1162    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ Priority v offset count
    1163    -> maker mkString        >>=            \v -> Just $ UNIMPLEMENTED $ CVSID v offset count
    1164    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ BLinkPkgID v offset count
    1165    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ BLinkHdrID v offset count
    1166    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ BLinkNEVRA v offset count
    1167    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ FLinkPkgID v offset count
    1168    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ FLinkHdrID v offset count
    1169    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ FLinkNEVRA v offset count
    1170    -> maker mkString        >>=            \v -> Just $ UNIMPLEMENTED $ PackageOrigin v offset count
    1171    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TriggerPreIn v offset count
    1172    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ UNIMPLEMENTED $ BuildSuggests v offset count
    1173    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ UNIMPLEMENTED $ BuildEnhances v offset count
    1174    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ ScriptStates v offset count
    1175    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ ScriptMetrics v offset count
    1176    -> maker mkWord32        >>= unlist >>= \v -> Just $ UNIMPLEMENTED $ BuildCPUClock v offset count
    1177    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ FileDigestAlgos v offset count
    1178    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ Variants v offset count
    1179    -> maker mkWord32        >>= unlist >>= \v -> Just $ UNIMPLEMENTED $ XMajor v offset count
    1180    -> maker mkWord32        >>= unlist >>= \v -> Just $ UNIMPLEMENTED $ XMinor v offset count
    1181    -> maker mkString        >>=            \v -> Just $ UNIMPLEMENTED $ RepoTag v offset count
    1182    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ Keywords v offset count
    1183    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ BuildPlatforms v offset count
    1184    -> maker mkWord32        >>= unlist >>= \v -> Just $ UNIMPLEMENTED $ PackageColor v offset count
    1185    -> maker mkWord32        >>= unlist >>= \v -> Just $ UNIMPLEMENTED $ PackagePrefColor v offset count
    1186    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ XattrsDict v offset count
    1187    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ FileXattrsx v offset count
    1188    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ DepAttrsDict v offset count
    1189    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ ConflictAttrsx v offset count
    1190    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ ObsoleteAttrsx v offset count
    1191    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ ProvideAttrsx v offset count
    1192    -> maker mkWord32        >>=            \v -> Just $ UNIMPLEMENTED $ RequireAttrsx v offset count
    1193    -> maker mkNull          >>=            \v -> Just $ UNIMPLEMENTED $ BuildProvides v offset count
    1194    -> maker mkNull          >>=            \v -> Just $ UNIMPLEMENTED $ BuildObsoletes v offset count
    1195    -> maker mkWord32        >>= unlist >>= \v -> Just $ DBInstance v offset count
    1196    -> maker mkString        >>=            \v -> Just $ NVRA v offset count

    5000    -> maker mkStringArray   >>=            \v -> Just $ FileNames v offset count
    5001    -> maker mkStringArray   >>=            \v -> Just $ FileProvide v offset count
    5002    -> maker mkStringArray   >>=            \v -> Just $ FileRequire v offset count
    5003    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ FSNames v offset count
    5004    -> maker mkWord64        >>=            \v -> Just $ UNIMPLEMENTED $ FSSizes v offset count
    5005    -> maker mkStringArray   >>=            \v -> Just $ TriggerConds v offset count
    5006    -> maker mkStringArray   >>=            \v -> Just $ TriggerType v offset count
    5007    -> maker mkStringArray   >>=            \v -> Just $ OrigFileNames v offset count
    5008    -> maker mkWord64        >>=            \v -> Just $ LongFileSizes v offset count
    5009    -> maker mkWord64        >>= unlist >>= \v -> Just $ LongSize v offset count
    5010    -> maker mkStringArray   >>=            \v -> Just $ FileCaps v offset count
    5011    -> maker mkWord32        >>= unlist >>= \v -> Just $ FileDigestAlgo v offset count
    5012    -> maker mkString        >>=            \v -> Just $ BugURL v offset count
    5013    -> maker mkString        >>=            \v -> Just $ EVR v offset count
    5014    -> maker mkString        >>=            \v -> Just $ NVR v offset count
    5015    -> maker mkString        >>=            \v -> Just $ NEVR v offset count
    5016    -> maker mkString        >>=            \v -> Just $ NEVRA v offset count
    5017    -> maker mkWord32        >>= unlist >>= \v -> Just $ HeaderColor v offset count
    5018    -> maker mkWord32        >>= unlist >>= \v -> Just $ Verbose v offset count
    5019    -> maker mkWord32        >>= unlist >>= \v -> Just $ EpochNum v offset count
    5020    -> maker mkWord32        >>= unlist >>= \v -> Just $ PreInFlags v offset count
    5021    -> maker mkWord32        >>= unlist >>= \v -> Just $ PostInFlags v offset count
    5022    -> maker mkWord32        >>= unlist >>= \v -> Just $ PreUnFlags v offset count
    5023    -> maker mkWord32        >>= unlist >>= \v -> Just $ PostUnFlags v offset count
    5024    -> maker mkWord32        >>= unlist >>= \v -> Just $ PreTransFlags v offset count
    5025    -> maker mkWord32        >>= unlist >>= \v -> Just $ PostTransFlags v offset count
    5026    -> maker mkWord32        >>= unlist >>= \v -> Just $ VerifyScriptFlags v offset count
    5027    -> maker mkWord32        >>=            \v -> Just $ TriggerScriptFlags v offset count
    5029    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ Collections v offset count
    5030    -> maker mkStringArray   >>=            \v -> Just $ PolicyNames v offset count
    5031    -> maker mkStringArray   >>=            \v -> Just $ PolicyTypes v offset count
    5032    -> maker mkWord32        >>=            \v -> Just $ PolicyTypesIndexes v offset count
    5033    -> maker mkWord32        >>=            \v -> Just $ PolicyFlags v offset count
    5034    -> maker mkString        >>=            \v -> Just $ PolicyVCS v offset count
    5035    -> maker mkStringArray   >>=            \v -> Just $ OrderName v offset count
    5036    -> maker mkStringArray   >>=            \v -> Just $ OrderVersion v offset count
    5037    -> maker mkWord32        >>=            \v -> Just $ OrderFlags v offset count
    5038    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ MSSFManifest v offset count
    5039    -> maker mkStringArray   >>=            \v -> Just $ UNIMPLEMENTED $ MSSFDomain v offset count
    5040    -> maker mkStringArray   >>=            \v -> Just $ InstFileNames v offset count
    5041    -> maker mkStringArray   >>=            \v -> Just $ RequireNEVRs v offset count
    5042    -> maker mkStringArray   >>=            \v -> Just $ ProvideNEVRs v offset count
    5043    -> maker mkStringArray   >>=            \v -> Just $ ObsoleteNEVRs v offset count
    5044    -> maker mkStringArray   >>=            \v -> Just $ ConflictNEVRs v offset count
    5045    -> maker mkWord32        >>=            \v -> Just $ FileNLinks v offset count
    5046    -> maker mkStringArray   >>=            \v -> Just $ RecommendName v offset count
    5047    -> maker mkStringArray   >>=            \v -> Just $ RecommendVersion v offset count
    5048    -> maker mkWord32        >>=            \v -> Just $ RecommendFlags v offset count
    5049    -> maker mkStringArray   >>=            \v -> Just $ SuggestName v offset count
    5050    -> maker mkStringArray   >>=            \v -> Just $ SuggestVersion v offset count
    5051    -> maker mkWord32        >>=            \v -> Just $ SuggestFlags v offset count
    5052    -> maker mkStringArray   >>=            \v -> Just $ SupplementName v offset count
    5053    -> maker mkStringArray   >>=            \v -> Just $ SupplementVersion v offset count
    5054    -> maker mkWord32        >>=            \v -> Just $ SupplementFlags v offset count
    5055    -> maker mkStringArray   >>=            \v -> Just $ EnhanceName v offset count
    5056    -> maker mkStringArray   >>=            \v -> Just $ EnhanceVersion v offset count
    5057    -> maker mkWord32        >>=            \v -> Just $ EnhanceFlags v offset count
    5058    -> maker mkStringArray   >>=            \v -> Just $ RecommendNEVRs v offset count
    5059    -> maker mkStringArray   >>=            \v -> Just $ SuggestNEVRs v offset count
    5060    -> maker mkStringArray   >>=            \v -> Just $ SupplementNEVRs v offset count
    5061    -> maker mkStringArray   >>=            \v -> Just $ EnhanceNEVRs v offset count
    5062    -> maker mkString        >>=            \v -> Just $ Encoding v offset count
    5063    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ FileTriggerIn v offset count
    5064    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ FileTriggerUn v offset count
    5065    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ FileTriggerPostUn v offset count
    5066    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerScripts v offset count
    5067    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerScriptProg v offset count
    5068    -> maker mkWord32        >>=            \v -> Just $ FileTriggerScriptFlags v offset count
    5069    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerName v offset count
    5070    -> maker mkWord32        >>=            \v -> Just $ FileTriggerIndex v offset count
    5071    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerVersion v offset count
    5072    -> maker mkWord32        >>=            \v -> Just $ FileTriggerFlags v offset count
    5073    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TransFileTriggerIn v offset count
    5074    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TransFileTriggerUn v offset count
    5075    -> maker mkNull          >>=            \v -> Just $ INTERNAL $ TransFileTriggerPostUn v offset count
    5076    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerScripts v offset count
    5077    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerScriptProg v offset count
    5078    -> maker mkWord32        >>=            \v -> Just $ TransFileTriggerScriptFlags v offset count
    5079    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerName v offset count
    5080    -> maker mkWord32        >>=            \v -> Just $ TransFileTriggerIndex v offset count
    5081    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerVersion v offset count
    5082    -> maker mkWord32        >>=            \v -> Just $ TransFileTriggerFlags v offset count
    5083    -> maker mkString        >>=            \v -> Just $ INTERNAL $ RemovePathPostFixes v offset count
    5084    -> maker mkWord32        >>=            \v -> Just $ FileTriggerPriorities v offset count
    5085    -> maker mkWord32        >>=            \v -> Just $ TransFileTriggerPriorities v offset count
    5086    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerConds v offset count
    5087    -> maker mkStringArray   >>=            \v -> Just $ FileTriggerType v offset count
    5088    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerConds v offset count
    5089    -> maker mkStringArray   >>=            \v -> Just $ TransFileTriggerType v offset count
    5090    -> maker mkStringArray   >>=            \v -> Just $ FileSignatures v offset count
    5091    -> maker mkWord32        >>= unlist >>= \v -> Just $ FileSignatureLength v offset count

    _       -> Nothing
 where
    maker fn = fn store ty offset count

    unlist :: [a] -> Maybe a
    unlist (a:_) = Just a
    unlist _     = Nothing

mkNull :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Null
mkNull _ ty _ _ | ty == 0    = Just Null
                | otherwise  = Nothing

mkChar :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Char]
mkChar store ty offset count | ty == 1   = Just $ C.unpack $ BS.take count' start
                             | otherwise = Nothing
 where
    count' = fromIntegral count
    start = BS.drop (fromIntegral offset) store

mkWord8 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word8]
mkWord8 store ty offset count | ty == 2      = Just $ readWords store 1 asWord8 offsets
                              | otherwise    = Nothing
 where
    offsets = take (fromIntegral count) [offset..]

mkWord16 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word16]
mkWord16 store ty offset count | ty == 3     = Just $ readWords store 2 asWord16 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*2)) [0 .. count-1]

mkWord32 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word32]
mkWord32 store ty offset count | ty == 4     = Just $ readWords store 4 asWord32 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*4)) [0 .. count-1]

mkWord64 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word64]
mkWord64 store ty offset count | ty == 5     = Just $ readWords store 8 asWord64 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*8)) [0 .. count-1]

mkString :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe String
mkString store ty offset _ | ty == 6   = Just $ C.unpack $ BS.takeWhile (/= 0) start
                           | otherwise = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkBinary :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe BS.ByteString
mkBinary store ty offset count | ty == 7     = Just $ BS.take count' start
                               | otherwise   = Nothing
 where
    count' = fromIntegral count
    start  = BS.drop (fromIntegral offset) store

mkStringArray :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [String]
mkStringArray store ty offset count | ty == 8    = Just $ map C.unpack $ readStrings start count
                                    | otherwise  = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkI18NString :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe BS.ByteString
mkI18NString store ty offset _ | ty == 9     = Just $ BS.takeWhile (/= 0) start
                               | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

-- I don't know how to split a ByteString up into chunks of a given size, so here's what I'm doing.  Take
-- a list of offsets of where in the ByteString to read.  Skip to each of those offsets, grab size bytes, and
-- convert those bytes into the type using the given conversion function.  Return that list.
readWords :: BS.ByteString -> Int -> (BS.ByteString -> a) -> [Word32] -> [a]
readWords bs size conv offsets = map (\offset -> conv $ BS.take size $ BS.drop (fromIntegral offset) bs) offsets

readStrings :: BS.ByteString -> Word32 -> [BS.ByteString]
readStrings bytestring count  = take (fromIntegral count) $ BS.split 0 bytestring

-- | Given a 'Tag' name and a list of 'Tag's, find the match and return it as a Maybe.
findTag :: String -> [Tag] -> Maybe Tag
findTag name = find (\t -> name == showConstr (toConstr t))

-- | Given a 'Tag' name and a list of 'Tag's, find the match, convert it into a
-- 'ByteString', and return it as a Maybe.
findByteStringTag :: String -> [Tag] -> Maybe BS.ByteString
findByteStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe BS.ByteString

-- | Given a 'Tag' name and a list of 'Tag's, find the match, convert it into a
-- String, and return it as a Maybe.
findStringTag :: String -> [Tag] -> Maybe String
findStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe String

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Strings, and return as a list.  If no results are found, return an empty list.
findStringListTag :: String -> [Tag] -> [String]
findStringListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [String]

-- | Given a 'Tag', return its type.
tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast
