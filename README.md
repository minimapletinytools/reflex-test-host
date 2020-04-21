# reflex-test-app

This library contains functionality for writing unit tests for the "model" portion of your reflex-frp apps. Please see `test/Reflex/Test/App.hs` for basic usage example.

Note that the following two reflex class constraints are not supported:

- `PostBuild` could be added easily, but I noticed different hosts have different semantics on how to treat `PostBuild`, i.e. is it something that runs just ONCE after the first setup or after each frame. To avoid confusion, this library does not support the constraint though it could be added easily. For example, see [reflex-basic-host](https://github.com/qfpl/reflex-basic-host/).

- `TriggerEvent` could also be added with some effort but breaks the "pureness" of the test (at least from its inputs) so I don't suggest it. You can at least test parts of your network that don't require `TriggerEvent`

This library is modified from `test/Test/Run.hs` in the [reflex main repository](https://github.com/reflex-frp/reflex). I'll deprecate this module if the functionality is ever moved into an exposed module which [I think it should be](https://github.com/reflex-frp/reflex/issues/412).
