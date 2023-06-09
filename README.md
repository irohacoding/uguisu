# Uguisu

A chat program with AI for GNU Emacs. houhokekyo!

![Screencast](screencast.gif)

## Prepare for use

### OpenAI API key

1\. Register [OpenAI](https://platform.openai.com/) and get the API key.

2\. Set environment variable: `OPENAI_API_KEY` to your `~/.profile` or where Emacs can load.

*Be careful not to publish the api key! and you may have to pay to use this API!*

ex. `export OPENAI_API_KEY="your-openai-api-key"`

3\. Log out of your computer and log back in again.

### curl command

If you don't install curl command, must install it before using this program.

ex. `$ sudo apt install curl`

### json package

This program use json package, so if you don't install it, run commands below.

ex. `M-x package-install` `RET` `json` `RET`

## Installation

1\. Download `uguisu.el` to your elisp directory: `~/.emacs.d/elisp/`

*There is no problem with any name for the directory name "elisp"*

2\. Load uguisu.el, write code below to your init.el: `~/.emacs.d/init.el`

```
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'uguisu)
```

## Usage

### Launch

Run command below, and then open \*uguisu\* buffer.

`M-x uguisu`

### Start chatting

Write a message below Form Feed (^L) line and type `RET` twice.

Waiting for a while, you will receive a reply.

## Customization

### Change AI model

`M-x customize-option` `RET` `uguisu-ai-model` `RET`

Open customize buffer and click `Value Menu`.

Select AI Model:

0. gpt-3.5-turbo
1. gpt-4

After selected, click `Apply and Save`!!
