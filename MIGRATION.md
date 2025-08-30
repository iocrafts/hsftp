# Module Namespace Migration

This migration introduces proper module namespacing to the hsftp library.

## Changes Made

### Module Structure
All modules have been moved from the top-level namespace to the `Hsftp.*` namespace:

- `CmdOptions` → `Hsftp.CmdOptions`
- `Commands` → `Hsftp.Commands`
- `Config` → `Hsftp.Config`
- `Options` → `Hsftp.Options`
- `Reader` → `Hsftp.Reader`
- `Util` → `Hsftp.Util`

### Directory Structure
```
src/
├── Hsftp/
│   ├── CmdOptions.hs
│   ├── Commands.hs
│   ├── Config.hs
│   ├── Options.hs
│   ├── Reader.hs
│   └── Util.hs
└── (old modules removed)
```

### Files Updated
- **hsftp.cabal**: Updated `exposed-modules` to use `Hsftp.*` namespace
- **app/Main.hs**: Updated all imports to use `Hsftp.*` modules
- **test/*.hs**: Updated all test files to import `Hsftp.*` modules

## Breaking Changes

⚠️ **This is a breaking change** for users of the hsftp library.

### Before (v1.4.1 and earlier)
```haskell
import Util (createFile, toDate, toEpoch)
import Commands (download, upload)
import Config (Config, mkConfig)
```

### After (v1.5.0+)
```haskell
import Hsftp.Util (createFile, toDate, toEpoch)
import Hsftp.Commands (download, upload)
import Hsftp.Config (Config, mkConfig)
```

## Benefits

1. **Prevents naming conflicts**: Modules like `Util` are very common and can conflict with other libraries
2. **Better organization**: Clear namespace hierarchy makes the library more professional
3. **Follows Haskell conventions**: Standard practice for Haskell libraries
4. **Future-proofing**: Easier to add more modules without conflicts

## Migration Guide

To update your code:

1. Add `Hsftp.` prefix to all module imports from this library
2. Update any qualified imports: `import qualified Util as U` becomes `import qualified Hsftp.Util as U`
3. No changes needed to function names or APIs - only import statements change

## Version Compatibility

- **v1.4.1 and earlier**: Old namespace (`Util`, `Commands`, etc.)
- **v1.5.0 and later**: New namespace (`Hsftp.Util`, `Hsftp.Commands`, etc.)

This addresses issue #16: "Name space for the library"