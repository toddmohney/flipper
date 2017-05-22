# examples/environment-config

See `Main.hs` for example usage.

The example can be run from the _flipper_ project root by executing these
commands from the CLI

```sh
# Example when no features are enabled
stack exec environment-config

# Example with the 'SOME_FEATURE' enabled
SOME_FEATURE=True stack exec environment-config

# Example with the 'SOME_OTHER_FEATURE' enabled
SOME_OTHER_FEATURE=True stack exec environment-config
```
