# ob-aider

An Org Babel library for sending prompts to an already running Aider.el comint buffer directly from Org mode source blocks.

## Overview

`ob-aider` allows you to interact with [aider.el](https://github.com/tninja/aider.el) directly from Org mode documents. This enables you to:

- Document your AI-assisted coding sessions in Org mode
- Send prompts to Aider from within your Org documents
- Capture Aider's responses as results in your Org document
- Create reproducible AI-assisted coding workflows

## Requirements

- Emacs 27.1 or later
- Org mode 9.4 or later
- [aider.el](https://github.com/tninja/aider.el)

## Installation

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/ob-aider.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/ob-aider")
   (require 'ob-aider)
   
   ;; Enable aider in org-babel
   (with-eval-after-load 'org
     (org-babel-do-load-languages
      'org-babel-load-languages
      (append org-babel-load-languages
              '((aider . t)))))
   ```

### Using straight.el and use-package

```elisp
(use-package ob-aider
  :straight (:host github :repo "yourusername/ob-aider")
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((aider . t)))))
```

### For Doom Emacs users

In your `~/.doom.d/packages.el` file, add:
```elisp
(package! ob-aider
  :recipe (:host github :repo "yourusername/ob-aider"))
```

In your `~/.doom.d/config.el` file, add:
```elisp
(use-package! ob-aider
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(aider . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
```

Then run `doom sync` to install the package.

## Usage

1. Start an Aider session using `M-x aider-start` or your preferred method.

2. In your Org document, create an Aider source block:
   ```org
   #+begin_src aider
   Please refactor this function to be more efficient...
   #+end_src
   ```

3. Execute the block with `C-c C-c` to send the prompt to the active Aider session.

4. The response from Aider will be captured and displayed as the result of the source block.

## Customization

You can customize the behavior of `ob-aider` through the following variables:

```elisp
;; Set timeout for waiting for Aider responses (in seconds)
(setq ob-aider-timeout 60)

;; Set delay between checks for Aider response completion
(setq ob-aider-response-delay 0.1)
```

## Troubleshooting

- **No active Aider conversation buffer found**: Make sure you have started an Aider session before executing an Aider source block.
- **Response timeout**: If Aider takes longer than expected to respond, you can increase the `ob-aider-timeout` value.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Acknowledgments

- [aider.el](https://github.com/tninja/aider.el) for the Aider Emacs integration
- [Org Babel](https://orgmode.org/worg/org-contrib/babel/) for the literate programming framework
