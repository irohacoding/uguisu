# Uguisu
A chat program with AI for GNU Emacs. houhokekyo!

## Prepare for use

### OpenAI API key

1\. Register [OpenAI](https://platform.openai.com/) and get the API key.

2\. Set valiable: `openai-api-key` to your init.el or where Emacs can load.

Be careful not to publish the api key!

### curl command

If you don't install curl command, must install it before using this program.

ex. `$ sudo apt install curl`

### json package

This program use json package, so if you don't install it, run commands below.

ex. `M-x package-install` `RET` `json` `RET`

## Installation

### Download

Download `uguisu.el` at GitHub.

### init.el

Load uguisu.el, write code below to your init.el: ~/.emacs.d/init.el

```
(require 'uguisu)
```

## Usage

### Launch

`M-x uguisu`

### Start chatting

Write a message below Form Feed (^L) line.

Waiting for a while, you will receive a reply.

## Customization

Nothing yet.
