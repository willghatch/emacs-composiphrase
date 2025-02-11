# Composiphrase

This is an emacs library for composing “command sentences” for execution.
You configure the names of verbs, objects, and modifiers, as well as which default modifiers are implied by any given verb or object if an explicit modifier is not given.
You also configure a match table that specifies how sentences are mapped to concrete commands (and what arguments are passed).
Then you bind keys that add any number of these words to the `composiphrase-current-sentence` and/or to `composiphrase-execute-current-sentence`.

There is a large demo configuration for the matcher [here](https://github.com/willghatch/emacs-composiphrase-demo/blob/master/composiphrase-demo-match-config.el), which I may turn into a default configuration (that you would set by calling some `use-default-config` function).

But more generally, [here](https://github.com/willghatch/emacs-composiphrase-demo) is an end-to-end demo of a composable editor using composiphrase.
The demo is vim-like, but taking the ideas of composability and text objects further, making an order of magnitude more operations at reach while also making it easier to memorize.
See [this blog post](https://willghatch.net/blog/text-editing/composiphrase_composable-editing-language-like-vim-but-moreso/) for more details.

