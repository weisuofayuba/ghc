module Oracles.Setting (
    configFile,
    -- * Settings
    Setting (..), SettingList (..), setting, settingList, getSetting,
    getSettingList,
    SettingsFileSetting (..), settingsFileSetting,

    -- * Helpers
    ghcCanonVersion, cmdLineLengthLimit, hostSupportsRPaths, topDirectory,
    libsuf, ghcVersionStage, bashPath,

    -- ** Target platform things
    anyTargetPlatform, anyTargetOs, anyTargetArch, anyHostOs,
    isElfTarget,
    ArmVersion(..),
    targetArmVersion,
    ghcWithInterpreter, useLibFFIForAdjustors
    ) where

import Hadrian.Expression
import Hadrian.Oracles.TextFile
import Hadrian.Oracles.Path

import Base

-- | Each 'Setting' comes from the file @hadrian/cfg/system.config@, generated
-- by the @configure@ script from the input file @hadrian/cfg/system.config.in@.
-- For example, the line
--
-- > target-os = mingw32
--
-- sets the value of the setting 'TargetOs'. The action 'setting' 'TargetOs'
-- looks up the value of the setting and returns the string @"mingw32"@,
-- tracking the result in the Shake database.
data Setting = BuildArch
             | BuildOs
             | BuildPlatform
             | BuildVendor
             | CursesLibDir
             | DynamicExtension
             | FfiIncludeDir
             | FfiLibDir
             | GhcMajorVersion
             | GhcMinorVersion
             | GhcPatchLevel
             | GhcVersion
             | GhcSourcePath
             | GmpIncludeDir
             | GmpLibDir
             | HostArch
             | HostOs
             | HostPlatform
             | HostVendor
             | HostArchHaskell
             | HostOsHaskell
             | IconvIncludeDir
             | IconvLibDir
             | LibdwIncludeDir
             | LibdwLibDir
             | LibnumaIncludeDir
             | LibnumaLibDir
             | LlvmTarget
             | ProjectGitCommitId
             | ProjectName
             | ProjectVersion
             | ProjectVersionInt
             | ProjectPatchLevel
             | ProjectPatchLevel1
             | ProjectPatchLevel2
             | SystemGhc
             | TargetArch
             | TargetOs
             | TargetPlatform
             | TargetPlatformFull
             | TargetVendor
             | TargetArchHaskell
             | TargetOsHaskell
             | TargetArmVersion
             | BourneShell

-- TODO: Reduce the variety of similar flags (e.g. CPP and non-CPP versions).
-- | Each 'SettingList' comes from the file @hadrian/cfg/system.config@,
-- generated by the @configure@ script from the input file
-- @hadrian/cfg/system.config.in@. For example, the line
--
-- > hs-cpp-args = -E -undef -traditional
--
-- sets the value of 'HsCppArgs'. The action 'settingList' 'HsCppArgs' looks up
-- the value of the setting and returns the list of strings
-- @["-E", "-undef", "-traditional"]@, tracking the result in the Shake database.
data SettingList = ConfCcArgs Stage
                 | ConfCppArgs Stage
                 | ConfGccLinkerArgs Stage
                 | ConfLdLinkerArgs Stage
                 | ConfMergeObjectsArgs Stage
                 | HsCppArgs

-- TODO compute solely in Hadrian, removing these variables' definitions
-- from aclocal.m4 whenever they can be calculated from other variables
-- already fed into Hadrian.

-- | Each 'SettingsFileSetting' is defined by 'FP_SETTINGS' in aclocal.m4.
-- Eventually much of that local can probably be computed just in Hadrian.
data SettingsFileSetting
    = SettingsFileSetting_CCompilerCommand
    | SettingsFileSetting_HaskellCPPCommand
    | SettingsFileSetting_HaskellCPPFlags
    | SettingsFileSetting_CCompilerFlags
    | SettingsFileSetting_CxxCompilerFlags
    | SettingsFileSetting_CCompilerLinkFlags
    | SettingsFileSetting_CCompilerSupportsNoPie
    | SettingsFileSetting_LdCommand
    | SettingsFileSetting_LdFlags
    | SettingsFileSetting_MergeObjectsCommand
    | SettingsFileSetting_MergeObjectsFlags
    | SettingsFileSetting_ArCommand
    | SettingsFileSetting_RanlibCommand
    | SettingsFileSetting_OtoolCommand
    | SettingsFileSetting_InstallNameToolCommand
    | SettingsFileSetting_DllWrapCommand
    | SettingsFileSetting_WindresCommand
    | SettingsFileSetting_LibtoolCommand
    | SettingsFileSetting_TouchCommand
    | SettingsFileSetting_ClangCommand
    | SettingsFileSetting_LlcCommand
    | SettingsFileSetting_OptCommand

-- | Look up the value of a 'Setting' in @cfg/system.config@, tracking the
-- result.
setting :: Setting -> Action String
setting key = lookupValueOrError configFile $ case key of
    BuildArch          -> "build-arch"
    BuildOs            -> "build-os"
    BuildPlatform      -> "build-platform"
    BuildVendor        -> "build-vendor"
    CursesLibDir       -> "curses-lib-dir"
    DynamicExtension   -> "dynamic-extension"
    FfiIncludeDir      -> "ffi-include-dir"
    FfiLibDir          -> "ffi-lib-dir"
    GhcMajorVersion    -> "ghc-major-version"
    GhcMinorVersion    -> "ghc-minor-version"
    GhcPatchLevel      -> "ghc-patch-level"
    GhcVersion         -> "ghc-version"
    GhcSourcePath      -> "ghc-source-path"
    GmpIncludeDir      -> "gmp-include-dir"
    GmpLibDir          -> "gmp-lib-dir"
    HostArch           -> "host-arch"
    HostOs             -> "host-os"
    HostPlatform       -> "host-platform"
    HostVendor         -> "host-vendor"
    HostArchHaskell    -> "host-arch-haskell"
    HostOsHaskell      -> "host-os-haskell"
    IconvIncludeDir    -> "iconv-include-dir"
    IconvLibDir        -> "iconv-lib-dir"
    LibdwIncludeDir    -> "libdw-include-dir"
    LibdwLibDir        -> "libdw-lib-dir"
    LibnumaIncludeDir  -> "libnuma-include-dir"
    LibnumaLibDir      -> "libnuma-lib-dir"
    LlvmTarget         -> "llvm-target"
    ProjectGitCommitId -> "project-git-commit-id"
    ProjectName        -> "project-name"
    ProjectVersion     -> "project-version"
    ProjectVersionInt  -> "project-version-int"
    ProjectPatchLevel  -> "project-patch-level"
    ProjectPatchLevel1 -> "project-patch-level1"
    ProjectPatchLevel2 -> "project-patch-level2"
    SystemGhc          -> "system-ghc"
    TargetArch         -> "target-arch"
    TargetArmVersion   -> "target-arm-version"
    TargetOs           -> "target-os"
    TargetPlatform     -> "target-platform"
    TargetPlatformFull -> "target-platform-full"
    TargetVendor       -> "target-vendor"
    TargetArchHaskell  -> "target-arch-haskell"
    TargetOsHaskell    -> "target-os-haskell"
    BourneShell        -> "bourne-shell"

-- | Look up the value of a 'SettingList' in @cfg/system.config@, tracking the
-- result.
settingList :: SettingList -> Action [String]
settingList key = fmap words $ lookupValueOrError configFile $ case key of
    ConfCcArgs        stage -> "conf-cc-args-"         ++ stageString stage
    ConfCppArgs       stage -> "conf-cpp-args-"        ++ stageString stage
    ConfGccLinkerArgs stage -> "conf-gcc-linker-args-" ++ stageString stage
    ConfLdLinkerArgs  stage -> "conf-ld-linker-args-"  ++ stageString stage
    ConfMergeObjectsArgs stage -> "conf-merge-objects-args-"  ++ stageString stage
    HsCppArgs               -> "hs-cpp-args"

-- | Look up the value of a 'SettingList' in @cfg/system.config@, tracking the
-- result.
-- See Note [tooldir: How GHC finds mingw on Windows]
settingsFileSetting :: SettingsFileSetting -> Action String
settingsFileSetting key = lookupValueOrError configFile $ case key of
    SettingsFileSetting_CCompilerCommand -> "settings-c-compiler-command"
    SettingsFileSetting_HaskellCPPCommand -> "settings-haskell-cpp-command"
    SettingsFileSetting_HaskellCPPFlags -> "settings-haskell-cpp-flags"
    SettingsFileSetting_CCompilerFlags -> "settings-c-compiler-flags"
    SettingsFileSetting_CxxCompilerFlags -> "settings-cxx-compiler-flags"
    SettingsFileSetting_CCompilerLinkFlags -> "settings-c-compiler-link-flags"
    SettingsFileSetting_CCompilerSupportsNoPie -> "settings-c-compiler-supports-no-pie"
    SettingsFileSetting_LdCommand -> "settings-ld-command"
    SettingsFileSetting_LdFlags -> "settings-ld-flags"
    SettingsFileSetting_MergeObjectsCommand -> "settings-merge-objects-command"
    SettingsFileSetting_MergeObjectsFlags -> "settings-merge-objects-flags"
    SettingsFileSetting_ArCommand -> "settings-ar-command"
    SettingsFileSetting_RanlibCommand -> "settings-ranlib-command"
    SettingsFileSetting_OtoolCommand -> "settings-otool-command"
    SettingsFileSetting_InstallNameToolCommand -> "settings-install_name_tool-command"
    SettingsFileSetting_DllWrapCommand -> "settings-dll-wrap-command"
    SettingsFileSetting_WindresCommand -> "settings-windres-command"
    SettingsFileSetting_LibtoolCommand -> "settings-libtool-command"
    SettingsFileSetting_TouchCommand -> "settings-touch-command"
    SettingsFileSetting_ClangCommand -> "settings-clang-command"
    SettingsFileSetting_LlcCommand -> "settings-llc-command"
    SettingsFileSetting_OptCommand -> "settings-opt-command"

-- | An expression that looks up the value of a 'Setting' in @cfg/system.config@,
-- tracking the result.
getSetting :: Setting -> Expr c b String
getSetting = expr . setting

-- | The path to a Bourne shell interpreter.
bashPath :: Action FilePath
bashPath = setting BourneShell

-- | An expression that looks up the value of a 'SettingList' in
-- @cfg/system.config@, tracking the result.
getSettingList :: SettingList -> Args c b
getSettingList = expr . settingList

-- | Check whether the value of a 'Setting' matches one of the given strings.
matchSetting :: Setting -> [String] -> Action Bool
matchSetting key values = (`elem` values) <$> setting key

-- | Check whether the target platform setting matches one of the given strings.
anyTargetPlatform :: [String] -> Action Bool
anyTargetPlatform = matchSetting TargetPlatformFull

-- | Check whether the target OS setting matches one of the given strings.
anyTargetOs :: [String] -> Action Bool
anyTargetOs = matchSetting TargetOs

-- | Check whether the target architecture setting matches one of the given
-- strings.
anyTargetArch :: [String] -> Action Bool
anyTargetArch = matchSetting TargetArch

-- | Check whether the host OS setting matches one of the given strings.
anyHostOs :: [String] -> Action Bool
anyHostOs = matchSetting HostOs

-- | Check whether the target OS uses the ELF object format.
isElfTarget :: Action Bool
isElfTarget = anyTargetOs
    [ "linux", "freebsd", "dragonfly", "openbsd", "netbsd", "solaris2", "kfreebsdgnu"
    , "haiku", "linux-android"
    ]

-- | Check whether the host OS supports the @-rpath@ linker option when
-- using dynamic linking.
--
-- TODO: Windows supports lazy binding (but GHC doesn't currently support
--       dynamic way on Windows anyways).
hostSupportsRPaths :: Action Bool
hostSupportsRPaths = anyHostOs ["linux", "darwin", "freebsd"]

-- | Check whether the target supports GHCi.
ghcWithInterpreter :: Action Bool
ghcWithInterpreter = do
    goodOs <- anyTargetOs [ "mingw32", "cygwin32", "linux", "solaris2"
                          , "freebsd", "dragonfly", "netbsd", "openbsd"
                          , "darwin", "kfreebsdgnu" ]
    goodArch <- anyTargetArch [ "i386", "x86_64", "powerpc", "sparc"
                              , "sparc64", "arm", "aarch64", "s390x"
                              , "powerpc64", "powerpc64le" ]
    return $ goodOs && goodArch

-- | Check to use @libffi@ for adjustors.
useLibFFIForAdjustors :: Action Bool
useLibFFIForAdjustors = notM $ anyTargetArch ["i386", "x86_64"]

-- | Variants of the ARM architecture.
data ArmVersion = ARMv5 | ARMv6 | ARMv7
                deriving (Eq, Ord, Show, Read)

-- | Which variant of the ARM architecture is the target (or 'Nothing' if not
-- ARM)?
targetArmVersion :: Action (Maybe ArmVersion)
targetArmVersion = do
  isArm <- anyTargetArch [ "arm" ]
  if isArm
    then Just . read <$> setting TargetArmVersion
    else return Nothing

-- | Canonicalised GHC version number, used for integer version comparisons. We
-- expand 'GhcMinorVersion' to two digits by adding a leading zero if necessary.
ghcCanonVersion :: Action String
ghcCanonVersion = do
    ghcMajorVersion <- setting GhcMajorVersion
    ghcMinorVersion <- setting GhcMinorVersion
    let leadingZero = [ '0' | length ghcMinorVersion == 1 ]
    return $ ghcMajorVersion ++ leadingZero ++ ghcMinorVersion

-- | Path to the GHC source tree.
topDirectory :: Action FilePath
topDirectory = fixAbsolutePathOnWindows =<< setting GhcSourcePath

ghcVersionStage :: Stage -> Action String
ghcVersionStage Stage0 = setting GhcVersion
ghcVersionStage _      = setting ProjectVersion

-- | The file suffix used for libraries of a given build 'Way'. For example,
-- @_p.a@ corresponds to a static profiled library, and @-ghc7.11.20141222.so@
-- is a dynamic vanilly library. Why do we need GHC version number in the
-- dynamic suffix? Here is a possible reason: dynamic libraries are placed in a
-- single giant directory in the load path of the dynamic linker, and hence we
-- must distinguish different versions of GHC. In contrast, static libraries
-- live in their own per-package directory and hence do not need a unique
-- filename. We also need to respect the system's dynamic extension, e.g. @.dll@
-- or @.so@.
libsuf :: Stage -> Way -> Action String
libsuf st way
    | not (wayUnit Dynamic way) = return (waySuffix way ++ ".a") -- e.g., _p.a
    | otherwise = do
        extension <- setting DynamicExtension -- e.g., .dll or .so
        version   <- ghcVersionStage st -- e.g. 8.4.4 or 8.9.xxxx
        let suffix = waySuffix (removeWayUnit Dynamic way)
        return (suffix ++ "-ghc" ++ version ++ extension)
