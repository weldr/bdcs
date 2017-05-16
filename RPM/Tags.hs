-- Copyright (C) 2016-2017 Red Hat, Inc.
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
                findWord16Tag,
                findWord16ListTag,
                findWord32Tag,
                findWord32ListTag,
                mkTag,
                tagValue)
 where

import           Data.Bits((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Data(Data, cast, gmapQi, showConstr, toConstr)
import           Data.List(find)
import           Data.Maybe(fromMaybe, listToMaybe)
import           Data.Typeable(Typeable)
import           Data.Word
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(text)

import RPM.Internal.Numbers

{-# ANN module "HLint: ignore Use camelCase" #-}

-- The character lists are actually lists of characters, ignore the suggestions
-- to use String instead
{-# ANN module "HLint: ignore Use String" #-}

data Tag = DEPRECATED                   Tag
         | INTERNAL                     Tag
         | OBSOLETE                     Tag
         | UNIMPLEMENTED                Tag
         | UNUSED                       Tag

         | HeaderImage                  Null
         | HeaderSignatures             Null
         | HeaderImmutable              Null
         | HeaderRegions                Null
         | HeaderI18NTable              [String]

         | SigBase                      Null
         | SigSize                      Word32
         | SigLEMD5_1                   Null
         | SigPGP                       BS.ByteString
         | SigLEMD5_2                   Null
         | SigMD5                       BS.ByteString
         | SigGPG                       BS.ByteString
         | SigPGP5                      Null
         | SigBadSHA1_1                 Null
         | SigBadSHA1_2                 Null
         | PubKeys                      [String]
         | DSAHeader                    BS.ByteString
         | RSAHeader                    BS.ByteString
         | SHA1Header                   String
         | LongSigSize                  Word64
         | LongArchiveSize              Word64

         | Name                         String
         | Version                      String
         | Release                      String
         | Epoch                        Word32
         | Summary                      BS.ByteString
         | Description                  BS.ByteString
         | BuildTime                    Word32
         | BuildHost                    String
         | InstallTime                  Word32
         | Size                         Word32
         | Distribution                 String
         | Vendor                       String
         | GIF                          BS.ByteString
         | XPM                          BS.ByteString
         | License                      String
         | Packager                     String
         | Group                        BS.ByteString
         | ChangeLog                    [String]
         | Source                       [String]
         | Patch                        [String]
         | URL                          String
         | OS                           String
         | Arch                         String
         | PreIn                        String
         | PostIn                       String
         | PreUn                        String
         | PostUn                       String
         | OldFileNames                 [String]
         | FileSizes                    [Word32]
         | FileStates                   [Char]
         | FileModes                    [Word16]
         | FileUIDs                     [Word32]
         | FileGIDs                     [Word32]
         | FileRDevs                    [Word16]
         | FileMTimes                   [Word32]
         | FileMD5s                     [String]
         | FileLinkTos                  [String]
         | FileFlags                    [Word32]
         | Root                         Null
         | FileUserName                 [String]
         | FileGroupName                [String]
         | Exclude                      Null
         | Exclusive                    Null
         | Icon                         BS.ByteString
         | SourceRPM                    String
         | FileVerifyFlags              [Word32]
         | ArchiveSize                  Word32
         | ProvideName                  [String]
         | RequireFlags                 [Word32]
         | RequireName                  [String]
         | RequireVersion               [String]
         | NoSource                     [Word32]
         | NoPatch                      [Word32]
         | ConflictFlags                [Word32]
         | ConflictName                 [String]
         | ConflictVersion              [String]
         | DefaultPrefix                String
         | BuildRoot                    String
         | InstallPrefix                String
         | ExcludeArch                  [String]
         | ExcludeOS                    [String]
         | ExclusiveArch                [String]
         | ExclusiveOS                  [String]
         | AutoReqProv                  String
         | RPMVersion                   String
         | TriggerScripts               [String]
         | TriggerName                  [String]
         | TriggerVersion               [String]
         | TriggerFlags                 [Word32]
         | TriggerIndex                 [Word32]
         | VerifyScript                 String
         | ChangeLogTime                [Word32]
         | ChangeLogName                [String]
         | ChangeLogText                [String]
         | BrokenMD5                    Null
         | PreReq                       Null
         | PreInProg                    [String]
         | PostInProg                   [String]
         | PreUnProg                    [String]
         | PostUnProg                   [String]
         | BuildArchs                   [String]
         | ObsoleteName                 [String]
         | VerifyScriptProg             [String]
         | TriggerScriptProg            [String]
         | DocDir                       Null
         | Cookie                       String
         | FileDevices                  [Word32]
         | FileINodes                   [Word32]
         | FileLangs                    [String]
         | Prefixes                     [String]
         | InstPrefixes                 [String]
         | TriggerIn                    Null
         | TriggerUn                    Null
         | TriggerPostUn                Null
         | AutoReq                      Null
         | AutoProv                     Null
         | Capability                   Word32
         | SourcePackage                Word32
         | OldOrigFileNames             Null
         | BuildPreReq                  Null
         | BuildRequires                Null
         | BuildConflicts               Null
         | BuildMacros                  Null
         | ProvideFlags                 [Word32]
         | ProvideVersion               [String]
         | ObsoleteFlags                [Word32]
         | ObsoleteVersion              [String]
         | DirIndexes                   [Word32]
         | BaseNames                    [String]
         | DirNames                     [String]
         | OrigDirIndexes               [Word32]
         | OrigBaseNames                [String]
         | OrigDirNames                 [String]
         | OptFlags                     String
         | DistURL                      String
         | PayloadFormat                String
         | PayloadCompressor            String
         | PayloadFlags                 String
         | InstallColor                 Word32
         | InstallTID                   Word32
         | RemoveTID                    Word32
         | SHA1RHN                      Null
         | RHNPlatform                  String
         | Platform                     String
         | PatchesName                  [String]
         | PatchesFlags                 [Word32]
         | PatchesVersion               [String]
         | CacheCTime                   Word32
         | CachePkgPath                 String
         | CachePkgSize                 Word32
         | CachePkgMTime                Word32
         | FileColors                   [Word32]
         | FileClass                    [Word32]
         | ClassDict                    [String]
         | FileDependsX                 [Word32]
         | FileDependsN                 [Word32]
         | DependsDict                  [(Word32, Word32)]
         | SourcePkgID                  BS.ByteString
         | FileContexts                 [String]
         | FSContexts                   [String]
         | ReContexts                   [String]
         | Policies                     [String]
         | PreTrans                     String
         | PostTrans                    String
         | PreTransProg                 [String]
         | PostTransProg                [String]
         | DistTag                      String
         | OldSuggestsName              [String]
         | OldSuggestsVersion           [String]
         | OldSuggestsFlags             [Word32]
         | OldEnhancesName              [String]
         | OldEnhancesVersion           [String]
         | OldEnhancesFlags             [Word32]
         | Priority                     [Word32]
         | CVSID                        String
         | BLinkPkgID                   [String]
         | BLinkHdrID                   [String]
         | BLinkNEVRA                   [String]
         | FLinkPkgID                   [String]
         | FLinkHdrID                   [String]
         | FLinkNEVRA                   [String]
         | PackageOrigin                String
         | TriggerPreIn                 Null
         | BuildSuggests                Null
         | BuildEnhances                Null
         | ScriptStates                 [Word32]
         | ScriptMetrics                [Word32]
         | BuildCPUClock                Word32
         | FileDigestAlgos              [Word32]
         | Variants                     [String]
         | XMajor                       Word32
         | XMinor                       Word32
         | RepoTag                      String
         | Keywords                     [String]
         | BuildPlatforms               [String]
         | PackageColor                 Word32
         | PackagePrefColor             Word32
         | XattrsDict                   [String]
         | FileXattrsx                  [Word32]
         | DepAttrsDict                 [String]
         | ConflictAttrsx               [Word32]
         | ObsoleteAttrsx               [Word32]
         | ProvideAttrsx                [Word32]
         | RequireAttrsx                [Word32]
         | BuildProvides                Null
         | BuildObsoletes               Null
         | DBInstance                   Word32
         | NVRA                         String

         | FileNames                    [String]
         | FileProvide                  [String]
         | FileRequire                  [String]
         | FSNames                      [String]
         | FSSizes                      [Word64]
         | TriggerConds                 [String]
         | TriggerType                  [String]
         | OrigFileNames                [String]
         | LongFileSizes                [Word64]
         | LongSize                     Word64
         | FileCaps                     [String]
         | FileDigestAlgo               Word32
         | BugURL                       String
         | EVR                          String
         | NVR                          String
         | NEVR                         String
         | NEVRA                        String
         | HeaderColor                  Word32
         | Verbose                      Word32
         | EpochNum                     Word32
         | PreInFlags                   Word32
         | PostInFlags                  Word32
         | PreUnFlags                   Word32
         | PostUnFlags                  Word32
         | PreTransFlags                Word32
         | PostTransFlags               Word32
         | VerifyScriptFlags            Word32
         | TriggerScriptFlags           [Word32]
         | Collections                  [String]
         | PolicyNames                  [String]
         | PolicyTypes                  [String]
         | PolicyTypesIndexes           [Word32]
         | PolicyFlags                  [Word32]
         | PolicyVCS                    String
         | OrderName                    [String]
         | OrderVersion                 [String]
         | OrderFlags                   [Word32]
         | MSSFManifest                 [String]
         | MSSFDomain                   [String]
         | InstFileNames                [String]
         | RequireNEVRs                 [String]
         | ProvideNEVRs                 [String]
         | ObsoleteNEVRs                [String]
         | ConflictNEVRs                [String]
         | FileNLinks                   [Word32]
         | RecommendName                [String]
         | RecommendVersion             [String]
         | RecommendFlags               [Word32]
         | SuggestName                  [String]
         | SuggestVersion               [String]
         | SuggestFlags                 [Word32]
         | SupplementName               [String]
         | SupplementVersion            [String]
         | SupplementFlags              [Word32]
         | EnhanceName                  [String]
         | EnhanceVersion               [String]
         | EnhanceFlags                 [Word32]
         | RecommendNEVRs               [String]
         | SuggestNEVRs                 [String]
         | SupplementNEVRs              [String]
         | EnhanceNEVRs                 [String]
         | Encoding                     String
         | FileTriggerIn                Null
         | FileTriggerUn                Null
         | FileTriggerPostUn            Null
         | FileTriggerScripts           [String]
         | FileTriggerScriptProg        [String]
         | FileTriggerScriptFlags       [Word32]
         | FileTriggerName              [String]
         | FileTriggerIndex             [Word32]
         | FileTriggerVersion           [String]
         | FileTriggerFlags             [Word32]
         | TransFileTriggerIn           Null
         | TransFileTriggerUn           Null
         | TransFileTriggerPostUn       Null
         | TransFileTriggerScripts      [String]
         | TransFileTriggerScriptProg   [String]
         | TransFileTriggerScriptFlags  [Word32]
         | TransFileTriggerName         [String]
         | TransFileTriggerIndex        [Word32]
         | TransFileTriggerVersion      [String]
         | TransFileTriggerFlags        [Word32]
         | RemovePathPostFixes          String
         | FileTriggerPriorities        [Word32]
         | TransFileTriggerPriorities   [Word32]
         | FileTriggerConds             [String]
         | FileTriggerType              [String]
         | TransFileTriggerConds        [String]
         | TransFileTriggerType         [String]
         | FileSignatures               [String]
         | FileSignatureLength          Word32
  deriving(Eq, Show, Data, Typeable)

instance Pretty Tag where
    -- This is a lot quicker than having to provide a Pretty instance that takes every
    -- single Tag into account.
    pPrint = text . show

data Null = Null
 deriving(Eq, Show, Data, Typeable)

mkTag :: BS.ByteString -> Int -> Word32 -> Word32 -> Word32 -> Maybe Tag
mkTag store tag ty offset count = case tag of
    61      -> maker mkNull          >>=                 Just . HeaderImage
    62      -> maker mkNull          >>=                 Just . HeaderSignatures
    63      -> maker mkNull          >>=                 Just . HeaderImmutable
    64      -> maker mkNull          >>=                 Just . HeaderRegions
    100     -> maker mkStringArray   >>=                 Just . HeaderI18NTable

    256     -> maker mkNull          >>=                 Just . SigBase
    257     -> maker mkWord32        >>= listToMaybe >>= Just . SigSize
    258     -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SigLEMD5_1
    259     -> maker mkBinary        >>=                 Just . SigPGP
    260     -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SigLEMD5_2
    261     -> maker mkBinary        >>=                 Just . SigMD5
    262     -> maker mkBinary        >>=                 Just . SigGPG
    263     -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SigPGP5
    264     -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SigBadSHA1_1
    265     -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SigBadSHA1_2
    266     -> maker mkStringArray   >>=                 Just . PubKeys
    267     -> maker mkBinary        >>=                 Just . DSAHeader
    268     -> maker mkBinary        >>=                 Just . RSAHeader
    269     -> maker mkString        >>=                 Just . SHA1Header
    270     -> maker mkWord64        >>= listToMaybe >>= Just . LongSigSize
    271     -> maker mkWord64        >>= listToMaybe >>= Just . LongArchiveSize

    1000    -> maker mkString        >>=                 Just . Name
    1001    -> maker mkString        >>=                 Just . Version
    1002    -> maker mkString        >>=                 Just . Release
    1003    -> maker mkWord32        >>= listToMaybe >>= Just . Epoch
    1004    -> maker mkI18NString    >>=                 Just . Summary
    1005    -> maker mkI18NString    >>=                 Just . Description
    1006    -> maker mkWord32        >>= listToMaybe >>= Just . BuildTime
    1007    -> maker mkString        >>=                 Just . BuildHost
    1008    -> maker mkWord32        >>= listToMaybe >>= Just . InstallTime
    1009    -> maker mkWord32        >>= listToMaybe >>= Just . Size
    1010    -> maker mkString        >>=                 Just . Distribution
    1011    -> maker mkString        >>=                 Just . Vendor
    1012    -> maker mkBinary        >>=                 Just . GIF
    1013    -> maker mkBinary        >>=                 Just . XPM
    1014    -> maker mkString        >>=                 Just . License
    1015    -> maker mkString        >>=                 Just . Packager
    1016    -> maker mkI18NString    >>=                 Just . Group
    1017    -> maker mkStringArray   >>=                 Just . INTERNAL . ChangeLog
    1018    -> maker mkStringArray   >>=                 Just . Source
    1019    -> maker mkStringArray   >>=                 Just . Patch
    1020    -> maker mkString        >>=                 Just . URL
    1021    -> maker mkString        >>=                 Just . OS
    1022    -> maker mkString        >>=                 Just . Arch
    1023    -> maker mkString        >>=                 Just . PreIn
    1024    -> maker mkString        >>=                 Just . PostIn
    1025    -> maker mkString        >>=                 Just . PreUn
    1026    -> maker mkString        >>=                 Just . PostUn
    1027    -> maker mkStringArray   >>=                 Just . OBSOLETE . OldFileNames
    1028    -> maker mkWord32        >>=                 Just . FileSizes
    1029    -> maker mkChar          >>=                 Just . FileStates
    1030    -> maker mkWord16        >>=                 Just . FileModes
    1031    -> maker mkWord32        >>=                 Just . INTERNAL . OBSOLETE . FileUIDs
    1032    -> maker mkWord32        >>=                 Just . INTERNAL . OBSOLETE . FileGIDs
    1033    -> maker mkWord16        >>=                 Just . FileRDevs
    1034    -> maker mkWord32        >>=                 Just . FileMTimes
    1035    -> maker mkStringArray   >>=                 Just . FileMD5s
    1036    -> maker mkStringArray   >>=                 Just . FileLinkTos
    1037    -> maker mkWord32        >>=                 Just . FileFlags
    1038    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . Root
    1039    -> maker mkStringArray   >>=                 Just . FileUserName
    1040    -> maker mkStringArray   >>=                 Just . FileGroupName
    1041    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . Exclude
    1042    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . Exclusive
    1043    -> maker mkBinary        >>=                 Just . Icon
    1044    -> maker mkString        >>=                 Just . SourceRPM
    1045    -> maker mkWord32        >>=                 Just . FileVerifyFlags
    1046    -> maker mkWord32        >>= listToMaybe >>= Just . ArchiveSize
    1047    -> maker mkStringArray   >>=                 Just . ProvideName
    1048    -> maker mkWord32        >>=                 Just . RequireFlags
    1049    -> maker mkStringArray   >>=                 Just . RequireName
    1050    -> maker mkStringArray   >>=                 Just . RequireVersion
    1051    -> maker mkWord32        >>=                 Just . NoSource
    1052    -> maker mkWord32        >>=                 Just . NoPatch
    1053    -> maker mkWord32        >>=                 Just . ConflictFlags
    1054    -> maker mkStringArray   >>=                 Just . ConflictName
    1055    -> maker mkStringArray   >>=                 Just . ConflictVersion
    1056    -> maker mkString        >>=                 Just . INTERNAL . DEPRECATED . DefaultPrefix
    1057    -> maker mkString        >>=                 Just . INTERNAL . OBSOLETE . BuildRoot
    1058    -> maker mkString        >>=                 Just . INTERNAL . DEPRECATED . InstallPrefix
    1059    -> maker mkStringArray   >>=                 Just . ExcludeArch
    1060    -> maker mkStringArray   >>=                 Just . ExcludeOS
    1061    -> maker mkStringArray   >>=                 Just . ExclusiveArch
    1062    -> maker mkStringArray   >>=                 Just . ExclusiveOS
    1063    -> maker mkString        >>=                 Just . INTERNAL . AutoReqProv
    1064    -> maker mkString        >>=                 Just . RPMVersion
    1065    -> maker mkStringArray   >>=                 Just . TriggerScripts
    1066    -> maker mkStringArray   >>=                 Just . TriggerName
    1067    -> maker mkStringArray   >>=                 Just . TriggerVersion
    1068    -> maker mkWord32        >>=                 Just . TriggerFlags
    1069    -> maker mkWord32        >>=                 Just . TriggerIndex
    1079    -> maker mkString        >>=                 Just . VerifyScript
    1080    -> maker mkWord32        >>=                 Just . ChangeLogTime
    1081    -> maker mkStringArray   >>=                 Just . ChangeLogName
    1082    -> maker mkStringArray   >>=                 Just . ChangeLogText
    1083    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . BrokenMD5
    1084    -> maker mkNull          >>=                 Just . INTERNAL . PreReq
    1085    -> maker mkStringArray   >>=                 Just . PreInProg
    1086    -> maker mkStringArray   >>=                 Just . PostInProg
    1087    -> maker mkStringArray   >>=                 Just . PreUnProg
    1088    -> maker mkStringArray   >>=                 Just . PostUnProg
    1089    -> maker mkStringArray   >>=                 Just . BuildArchs
    1090    -> maker mkStringArray   >>=                 Just . ObsoleteName
    1091    -> maker mkStringArray   >>=                 Just . VerifyScriptProg
    1092    -> maker mkStringArray   >>=                 Just . TriggerScriptProg
    1093    -> maker mkNull          >>=                 Just . INTERNAL . DocDir
    1094    -> maker mkString        >>=                 Just . Cookie
    1095    -> maker mkWord32        >>=                 Just . FileDevices
    1096    -> maker mkWord32        >>=                 Just . FileINodes
    1097    -> maker mkStringArray   >>=                 Just . FileLangs
    1098    -> maker mkStringArray   >>=                 Just . Prefixes
    1099    -> maker mkStringArray   >>=                 Just . InstPrefixes
    1100    -> maker mkNull          >>=                 Just . INTERNAL . TriggerIn
    1101    -> maker mkNull          >>=                 Just . INTERNAL . TriggerUn
    1102    -> maker mkNull          >>=                 Just . INTERNAL . TriggerPostUn
    1103    -> maker mkNull          >>=                 Just . INTERNAL . AutoReq
    1104    -> maker mkNull          >>=                 Just . INTERNAL . AutoProv
    1105    -> maker mkWord32        >>= listToMaybe >>= Just . INTERNAL . OBSOLETE . Capability
    1106    -> maker mkWord32        >>= listToMaybe >>= Just . SourcePackage
    1107    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . OldOrigFileNames
    1108    -> maker mkNull          >>=                 Just . INTERNAL . BuildPreReq
    1109    -> maker mkNull          >>=                 Just . INTERNAL . BuildRequires
    1110    -> maker mkNull          >>=                 Just . INTERNAL . BuildConflicts
    1111    -> maker mkNull          >>=                 Just . INTERNAL . UNUSED . BuildMacros
    1112    -> maker mkWord32        >>=                 Just . ProvideFlags
    1113    -> maker mkStringArray   >>=                 Just . ProvideVersion
    1114    -> maker mkWord32        >>=                 Just . ObsoleteFlags
    1115    -> maker mkStringArray   >>=                 Just . ObsoleteVersion
    1116    -> maker mkWord32        >>=                 Just . DirIndexes
    1117    -> maker mkStringArray   >>=                 Just . BaseNames
    1118    -> maker mkStringArray   >>=                 Just . DirNames
    1119    -> maker mkWord32        >>=                 Just . OrigDirIndexes
    1120    -> maker mkStringArray   >>=                 Just . OrigBaseNames
    1121    -> maker mkStringArray   >>=                 Just . OrigDirNames
    1122    -> maker mkString        >>=                 Just . OptFlags
    1123    -> maker mkString        >>=                 Just . DistURL
    1124    -> maker mkString        >>=                 Just . PayloadFormat
    1125    -> maker mkString        >>=                 Just . PayloadCompressor
    1126    -> maker mkString        >>=                 Just . PayloadFlags
    1127    -> maker mkWord32        >>= listToMaybe >>= Just . InstallColor
    1128    -> maker mkWord32        >>= listToMaybe >>= Just . InstallTID
    1129    -> maker mkWord32        >>= listToMaybe >>= Just . RemoveTID
    1130    -> maker mkNull          >>=                 Just . INTERNAL . OBSOLETE . SHA1RHN
    1131    -> maker mkString        >>=                 Just . INTERNAL . OBSOLETE . RHNPlatform
    1132    -> maker mkString        >>=                 Just . Platform
    1133    -> maker mkStringArray   >>=                 Just . DEPRECATED . PatchesName
    1134    -> maker mkWord32        >>=                 Just . DEPRECATED . PatchesFlags
    1135    -> maker mkStringArray   >>=                 Just . DEPRECATED . PatchesVersion
    1136    -> maker mkWord32        >>= listToMaybe >>= Just . INTERNAL . OBSOLETE . CacheCTime
    1137    -> maker mkString        >>=                 Just . INTERNAL . OBSOLETE . CachePkgPath
    1138    -> maker mkWord32        >>= listToMaybe >>= Just . INTERNAL . OBSOLETE . CachePkgSize
    1139    -> maker mkWord32        >>= listToMaybe >>= Just . INTERNAL . OBSOLETE . CachePkgMTime
    1140    -> maker mkWord32        >>=                 Just . FileColors
    1141    -> maker mkWord32        >>=                 Just . FileClass
    1142    -> maker mkStringArray   >>=                 Just . ClassDict
    1143    -> maker mkWord32        >>=                 Just . FileDependsX
    1144    -> maker mkWord32        >>=                 Just . FileDependsN
    1145    -> maker mkWord32        >>=                 Just . DependsDict . map (\x -> ((x `shiftR` 24) .&. 0xff, x .&. 0x00ffffff))
    1146    -> maker mkBinary        >>=                 Just . SourcePkgID
    1147    -> maker mkStringArray   >>=                 Just . OBSOLETE . FileContexts
    1148    -> maker mkStringArray   >>=                 Just . FSContexts
    1149    -> maker mkStringArray   >>=                 Just . ReContexts
    1150    -> maker mkStringArray   >>=                 Just . Policies
    1151    -> maker mkString        >>=                 Just . PreTrans
    1152    -> maker mkString        >>=                 Just . PostTrans
    1153    -> maker mkStringArray   >>=                 Just . PreTransProg
    1154    -> maker mkStringArray   >>=                 Just . PostTransProg
    1155    -> maker mkString        >>=                 Just . DistTag
    1156    -> maker mkStringArray   >>=                 Just . OBSOLETE . OldSuggestsName
    1157    -> maker mkStringArray   >>=                 Just . OBSOLETE . OldSuggestsVersion
    1158    -> maker mkWord32        >>=                 Just . OBSOLETE . OldSuggestsFlags
    1159    -> maker mkStringArray   >>=                 Just . OBSOLETE . OldEnhancesName
    1160    -> maker mkStringArray   >>=                 Just . OBSOLETE . OldEnhancesVersion
    1161    -> maker mkWord32        >>=                 Just . OBSOLETE . OldEnhancesFlags
    1162    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . Priority
    1163    -> maker mkString        >>=                 Just . UNIMPLEMENTED . CVSID
    1164    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . BLinkPkgID
    1165    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . BLinkHdrID
    1166    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . BLinkNEVRA
    1167    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . FLinkPkgID
    1168    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . FLinkHdrID
    1169    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . FLinkNEVRA
    1170    -> maker mkString        >>=                 Just . UNIMPLEMENTED . PackageOrigin
    1171    -> maker mkNull          >>=                 Just . INTERNAL . TriggerPreIn
    1172    -> maker mkNull          >>=                 Just . INTERNAL . UNIMPLEMENTED . BuildSuggests
    1173    -> maker mkNull          >>=                 Just . INTERNAL . UNIMPLEMENTED . BuildEnhances
    1174    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . ScriptStates
    1175    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . ScriptMetrics
    1176    -> maker mkWord32        >>= listToMaybe >>= Just . UNIMPLEMENTED . BuildCPUClock
    1177    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . FileDigestAlgos
    1178    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . Variants
    1179    -> maker mkWord32        >>= listToMaybe >>= Just . UNIMPLEMENTED . XMajor
    1180    -> maker mkWord32        >>= listToMaybe >>= Just . UNIMPLEMENTED . XMinor
    1181    -> maker mkString        >>=                 Just . UNIMPLEMENTED . RepoTag
    1182    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . Keywords
    1183    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . BuildPlatforms
    1184    -> maker mkWord32        >>= listToMaybe >>= Just . UNIMPLEMENTED . PackageColor
    1185    -> maker mkWord32        >>= listToMaybe >>= Just . UNIMPLEMENTED . PackagePrefColor
    1186    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . XattrsDict
    1187    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . FileXattrsx
    1188    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . DepAttrsDict
    1189    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . ConflictAttrsx
    1190    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . ObsoleteAttrsx
    1191    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . ProvideAttrsx
    1192    -> maker mkWord32        >>=                 Just . UNIMPLEMENTED . RequireAttrsx
    1193    -> maker mkNull          >>=                 Just . UNIMPLEMENTED . BuildProvides
    1194    -> maker mkNull          >>=                 Just . UNIMPLEMENTED . BuildObsoletes
    1195    -> maker mkWord32        >>= listToMaybe >>= Just . DBInstance
    1196    -> maker mkString        >>=                 Just . NVRA

    5000    -> maker mkStringArray   >>=                 Just . FileNames
    5001    -> maker mkStringArray   >>=                 Just . FileProvide
    5002    -> maker mkStringArray   >>=                 Just . FileRequire
    5003    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . FSNames
    5004    -> maker mkWord64        >>=                 Just . UNIMPLEMENTED . FSSizes
    5005    -> maker mkStringArray   >>=                 Just . TriggerConds
    5006    -> maker mkStringArray   >>=                 Just . TriggerType
    5007    -> maker mkStringArray   >>=                 Just . OrigFileNames
    5008    -> maker mkWord64        >>=                 Just . LongFileSizes
    5009    -> maker mkWord64        >>= listToMaybe >>= Just . LongSize
    5010    -> maker mkStringArray   >>=                 Just . FileCaps
    5011    -> maker mkWord32        >>= listToMaybe >>= Just . FileDigestAlgo
    5012    -> maker mkString        >>=                 Just . BugURL
    5013    -> maker mkString        >>=                 Just . EVR
    5014    -> maker mkString        >>=                 Just . NVR
    5015    -> maker mkString        >>=                 Just . NEVR
    5016    -> maker mkString        >>=                 Just . NEVRA
    5017    -> maker mkWord32        >>= listToMaybe >>= Just . HeaderColor
    5018    -> maker mkWord32        >>= listToMaybe >>= Just . Verbose
    5019    -> maker mkWord32        >>= listToMaybe >>= Just . EpochNum
    5020    -> maker mkWord32        >>= listToMaybe >>= Just . PreInFlags
    5021    -> maker mkWord32        >>= listToMaybe >>= Just . PostInFlags
    5022    -> maker mkWord32        >>= listToMaybe >>= Just . PreUnFlags
    5023    -> maker mkWord32        >>= listToMaybe >>= Just . PostUnFlags
    5024    -> maker mkWord32        >>= listToMaybe >>= Just . PreTransFlags
    5025    -> maker mkWord32        >>= listToMaybe >>= Just . PostTransFlags
    5026    -> maker mkWord32        >>= listToMaybe >>= Just . VerifyScriptFlags
    5027    -> maker mkWord32        >>=                 Just . TriggerScriptFlags
    5029    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . Collections
    5030    -> maker mkStringArray   >>=                 Just . PolicyNames
    5031    -> maker mkStringArray   >>=                 Just . PolicyTypes
    5032    -> maker mkWord32        >>=                 Just . PolicyTypesIndexes
    5033    -> maker mkWord32        >>=                 Just . PolicyFlags
    5034    -> maker mkString        >>=                 Just . PolicyVCS
    5035    -> maker mkStringArray   >>=                 Just . OrderName
    5036    -> maker mkStringArray   >>=                 Just . OrderVersion
    5037    -> maker mkWord32        >>=                 Just . OrderFlags
    5038    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . MSSFManifest
    5039    -> maker mkStringArray   >>=                 Just . UNIMPLEMENTED . MSSFDomain
    5040    -> maker mkStringArray   >>=                 Just . InstFileNames
    5041    -> maker mkStringArray   >>=                 Just . RequireNEVRs
    5042    -> maker mkStringArray   >>=                 Just . ProvideNEVRs
    5043    -> maker mkStringArray   >>=                 Just . ObsoleteNEVRs
    5044    -> maker mkStringArray   >>=                 Just . ConflictNEVRs
    5045    -> maker mkWord32        >>=                 Just . FileNLinks
    5046    -> maker mkStringArray   >>=                 Just . RecommendName
    5047    -> maker mkStringArray   >>=                 Just . RecommendVersion
    5048    -> maker mkWord32        >>=                 Just . RecommendFlags
    5049    -> maker mkStringArray   >>=                 Just . SuggestName
    5050    -> maker mkStringArray   >>=                 Just . SuggestVersion
    5051    -> maker mkWord32        >>=                 Just . SuggestFlags
    5052    -> maker mkStringArray   >>=                 Just . SupplementName
    5053    -> maker mkStringArray   >>=                 Just . SupplementVersion
    5054    -> maker mkWord32        >>=                 Just . SupplementFlags
    5055    -> maker mkStringArray   >>=                 Just . EnhanceName
    5056    -> maker mkStringArray   >>=                 Just . EnhanceVersion
    5057    -> maker mkWord32        >>=                 Just . EnhanceFlags
    5058    -> maker mkStringArray   >>=                 Just . RecommendNEVRs
    5059    -> maker mkStringArray   >>=                 Just . SuggestNEVRs
    5060    -> maker mkStringArray   >>=                 Just . SupplementNEVRs
    5061    -> maker mkStringArray   >>=                 Just . EnhanceNEVRs
    5062    -> maker mkString        >>=                 Just . Encoding
    5063    -> maker mkNull          >>=                 Just . INTERNAL . FileTriggerIn
    5064    -> maker mkNull          >>=                 Just . INTERNAL . FileTriggerUn
    5065    -> maker mkNull          >>=                 Just . INTERNAL . FileTriggerPostUn
    5066    -> maker mkStringArray   >>=                 Just . FileTriggerScripts
    5067    -> maker mkStringArray   >>=                 Just . FileTriggerScriptProg
    5068    -> maker mkWord32        >>=                 Just . FileTriggerScriptFlags
    5069    -> maker mkStringArray   >>=                 Just . FileTriggerName
    5070    -> maker mkWord32        >>=                 Just . FileTriggerIndex
    5071    -> maker mkStringArray   >>=                 Just . FileTriggerVersion
    5072    -> maker mkWord32        >>=                 Just . FileTriggerFlags
    5073    -> maker mkNull          >>=                 Just . INTERNAL . TransFileTriggerIn
    5074    -> maker mkNull          >>=                 Just . INTERNAL . TransFileTriggerUn
    5075    -> maker mkNull          >>=                 Just . INTERNAL . TransFileTriggerPostUn
    5076    -> maker mkStringArray   >>=                 Just . TransFileTriggerScripts
    5077    -> maker mkStringArray   >>=                 Just . TransFileTriggerScriptProg
    5078    -> maker mkWord32        >>=                 Just . TransFileTriggerScriptFlags
    5079    -> maker mkStringArray   >>=                 Just . TransFileTriggerName
    5080    -> maker mkWord32        >>=                 Just . TransFileTriggerIndex
    5081    -> maker mkStringArray   >>=                 Just . TransFileTriggerVersion
    5082    -> maker mkWord32        >>=                 Just . TransFileTriggerFlags
    5083    -> maker mkString        >>=                 Just . INTERNAL . RemovePathPostFixes
    5084    -> maker mkWord32        >>=                 Just . FileTriggerPriorities
    5085    -> maker mkWord32        >>=                 Just . TransFileTriggerPriorities
    5086    -> maker mkStringArray   >>=                 Just . FileTriggerConds
    5087    -> maker mkStringArray   >>=                 Just . FileTriggerType
    5088    -> maker mkStringArray   >>=                 Just . TransFileTriggerConds
    5089    -> maker mkStringArray   >>=                 Just . TransFileTriggerType
    5090    -> maker mkStringArray   >>=                 Just . FileSignatures
    5091    -> maker mkWord32        >>= listToMaybe >>= Just . FileSignatureLength

    _       -> Nothing
 where
    maker fn = fn store ty offset count

mkNull :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Null
mkNull _ ty _ _ | ty == 0    = Just Null
                | otherwise  = Nothing

mkChar :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Char]
mkChar store ty offset count | ty == 1   = Just $ C.unpack $ BS.take count' start
                             | otherwise = Nothing
 where
    count' = fromIntegral count
    start = BS.drop (fromIntegral offset) store

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
{-# ANN readWords "HLint: ignore Eta reduce" #-}
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

-- | Given a 'Tag' name and a list of 'Tag's, find the match convert it into a
-- Word16, and return it as a Maybe.
findWord16Tag :: String -> [Tag] -> Maybe Word16
findWord16Tag name tags = findTag name tags >>= \t -> tagValue t :: Maybe Word16

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Word16, and return as a list.  if no results are found, return an empty list.
findWord16ListTag :: String -> [Tag] -> [Word16]
findWord16ListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [Word16]

-- | Given a 'Tag' name and a list of 'Tag's, find the match convert it into a
-- Word32, and return it as a Maybe.
findWord32Tag :: String -> [Tag] -> Maybe Word32
findWord32Tag name tags = findTag name tags >>= \t -> tagValue t :: Maybe Word32

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Word32, and return as a list.  if no results are found, return an empty list.
findWord32ListTag :: String -> [Tag] -> [Word32]
findWord32ListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [Word32]

-- | Given a 'Tag', return its type.
tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast
