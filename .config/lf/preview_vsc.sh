#!/bin/sh

case "$(printf "%s\n" "$(readlink -f "$1")" | awk '{print tolower($0)}')" in
	*.pdf) pdftotext "$1" - | highlight -O ansi --force ;;
	*.tgz|*.tar.gz) tar tzf "$1" ;;
        *.tar.bz2|*.tbz2) tar tjf "$1" ;;
        *.tar.txz|*.txz) xz --list "$1" ;;
        *.tar) tar tf "$1" ;;
        *.zip|*.jar|*.war|*.ear|*.oxt) unzip -l "$1" ;;
        *.rar) unrar l "$1" ;;
        *.7z) 7za l "$1" ;;
        *.[1-8]) man "$1" | col -b ;;
        #*) highlight -O ansi --force "$1" ;;
        *) highlight -O ansi --force "$1" && exit 0
           $PAGER "$1" && exit 0
           exit 1
           ;;
esac
exit 0
