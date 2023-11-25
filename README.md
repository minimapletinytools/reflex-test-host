# reflex-test-host

This library contains functionality for writing unit tests for the "model" portion of your reflex-frp apps. Please see "test/Reflex/Test/SimpleHostSpec.hs" for basic usage example. You can find more usage examples in [this project](https://github.com/minimapletinytools/reflex-potatoes?files=1) and [this one](https://github.com/minimapletinytools/reflex-todo-undo-mvc-model).

Note that `TriggerEvent` class constraint is not supported. It could be added with some effort but breaks the "pureness" of the test (at least from its inputs) so I don't suggest it. You can still test parts of your network that don't require `TriggerEvent`.

There is also a monadic testing module in this library. You can see usage examples in "test/Reflex/Test/Monad/HostSpec.hs". 

PRs are welcome. Perhaps you have some ideas to make the interface more useable.
