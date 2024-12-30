# isgd.el

## Introduction

`isgd.el` will simply shorten your url with the is.gd service.

## Usage

Use `(isgd-shorten URL)` from an Emacs Lisp program, or
`M-x isgd-copy-url-at-point` to copy the shortened URL at point (or the region)
to the kill ring, or `M-x isgd-replace-url-at-point` to replace the URL at point
(or the region) with a shortened version.

## Installation

```emacs-lisp
(use-package isgd
  :ensure t
  :custom
  (isgd-logstats nil)
  (isgd-ask-custom-url t))
```

## Customization

- `isgd-base-url' is the base URL for the is.gd shortening service API.
- `isgd-logstats' enables detailed logging of statistics for shortened URLs.
- `isgd-ask-custom-url' asks for a custom short URL when shortening URLs.

## License

Apache 2.0

## Copyright

[Apache-2.0](./LICENSE)

## Authors

### Chmouel Boudjnah

* Fediverse - <[@chmouel@chmouel.com](https://fosstodon.org/@chmouel)>
* Twitter - <[@chmouel](https://twitter.com/chmouel)>
* Blog  - <[https://blog.chmouel.com](https://blog.chmouel.com)>
