**Double Commander** is a [free](https://www.gnu.org/philosophy/free-sw.html) cross-platform open source file manager with two panels side by side (or one above the other). It is inspired by Total Commander and features some innovative new ideas. 

Double Commander can be run on several platforms and operating systems.
It supports 32-bit and 64-bit processors. See [Supported platforms](https://github.com/doublecmd/doublecmd/wiki/Supported-platforms)
for a complete list.

See Double Commander in action in the [Screenshot Gallery](https://doublecmd.sourceforge.io/gallery).

## Where to start

### Download

Go to the [Double Commander download page](https://sourceforge.net/p/doublecmd/wiki/Download) to download the latest release.

You can check the latest version on the [Versions](https://github.com/doublecmd/doublecmd/wiki/Versions) page.

See if Double Commander is supported for your platform on the [Supported
platforms](https://github.com/doublecmd/doublecmd/wiki/Supported-platforms) page.

### Nix

The project provides optional Nix flake outputs for users who already use Nix. The flake wraps the prebuilt release binary, providing access to all GitHub releases (including beta releases that may not yet be in nixpkgs).

```bash
# Run without installing
nix run github:doublecmd/doublecmd

# Install into your profile
nix profile install github:doublecmd/doublecmd
```

The flake tracks the default branch and is auto-bumped to the latest release by a
daily [workflow](.github/workflows/nix-release.yml), so `github:doublecmd/doublecmd`
always serves the current release. (Release tags are cut before the bump lands,
so `github:doublecmd/doublecmd/vX.Y.Z` is not a valid pin — use the
nixpkgs package or a specific commit SHA if you need reproducibility.)

### Devbox

For reproducible development environments, use Devbox:

```bash
# Install Devbox first (if not already installed)
curl -fsSL https://get.jetify.dev/devbox | bash

# Initialize the environment
devbox shell

# Build the project
devbox run build
```

Or install Devbox via Homebrew:

```bash
brew install jetify-com/devbox/devbox
```

### Develop

For more information on the development of Double Commander,
see the [Development](https://github.com/doublecmd/doublecmd/wiki/Development) page.

### Discuss
  
Go to our [forum](https://doublecmd.h1n.ru) for discussions. There are English and Russian sections.

If you want to stay up-to-date with the project, you can check out the available [news feeds](https://github.com/doublecmd/doublecmd/wiki/News-feeds).
