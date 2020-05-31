# reflex-test-host

This library contains functionality for writing unit tests for the "model" portion of your reflex-frp apps. Please see `test/Reflex/Test/App.hs` for basic usage example. You can find more usage examples in [this project](https://github.com/pdlla/reflex-potatoes?files=1) and [this one](https://github.com/pdlla/reflex-todo-undo-mvc-model).

Note that `TriggerEvent` class constraint is not supported. It could be added with some effort but breaks the "pureness" of the test (at least from its inputs) so I don't suggest it. You can still test parts of your network that don't require `TriggerEvent`.

This library is modified from `test/Test/Run.hs` in the [reflex main repository](https://github.com/reflex-frp/reflex). I'll deprecate this module if the functionality is ever moved into an exposed module which [I think it should be](https://github.com/reflex-frp/reflex/issues/412).

PRs are welcome. Perhaps you have some ideas to make the interface more useable.
