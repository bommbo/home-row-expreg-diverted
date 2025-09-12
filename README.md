# home-row-expreg-diverted

Return to **exactly where you started** after any command that follows a `home-row-expreg-expand-with-letters` expansion — no keystrokes, no thinking.

Hit C-g to cancel? You'll land back home just the same.

---

## Dependencies

- [diverted](https://github.com/xenodium/diverted)

---

## Installation

```elisp
(use-package home-row-expreg-diverted
  :straight (:host github :repo "bommbo/home-row-expreg-diverted")
  :config
  ;; (add-to-list 'home-row-expreg-diverted-commands 'meow-save)
  ;; (add-to-list 'home-row-expreg-diverted-commands 'indent-for-tab-command)

  (home-row-expreg-diverted-mode 1))
```
