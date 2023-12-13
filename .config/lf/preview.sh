#!/bin/sh

# file preview handler for lf, all text.

set -C -f
IFS="$(printf '%b_' '\n')"; IFS="${IFS%_}"

# note that the cache file name is a function of file information, meaning if
# an image appears in multiple places across the machine, it will not have to
# be regenerated once seen.

path="$1"
file="${path##*/}"
mime="$(file --dereference --brief --mime-type -- "$1")"
name="${file%%.*}" # shell escape
ext="${file#*.}" # shell escape; "example.tar.gz" gives "tar.gz"

case $mime in

    ## debug
    # *)
    #   echo $mime
    #   ;;

    image/*)
        exiv2 "$1"
        ;;

    pdf)
        ## preview as text conversion
        pdftotext -l 10 -nopgbrk -q -- "$1" - | fmt -w "$(($4-2))"
        ;;

    *opendocument*)
        odt2txt "$1"
        ;;

    ## rtf and doc
    text/rtf|*msword)
        ## preview as text conversion
        ## note: catdoc does not always work for .doc files
        ## catdoc: http://www.wagner.pp.ru/~vitus/software/catdoc/
        catdoc -- "$1"
        ;;

    ## docx, epub, fb2 (using markdown)
    ## you might want to remove "|epub" and/or "|fb2" below if you have
    ## uncommented other methods to preview those formats
    #*wordprocessingml.document*/epub+zip|*/x-fictionbook+xml)
    *wordprocessingml.document*)
        ## preview as markdown conversion
        pandoc -s -t markdown -- "$1"
        ;;

    ## xls
    *ms-excel)
        ## preview as csv conversion
        ## xls2csv comes with catdoc:
        ##   http://www.wagner.pp.ru/~vitus/software/catdoc/
        xls2csv -- "$1"
        ;;

    application/*zip)
        atool --list -- "$1"
        ;;

    application/pgp-encrypted)
        gpg -d -- "$1"
        ;;

    ## executables and shared objects
    application/x-executable | application/x-pie-executable | application/x-sharedlib)
        readelf -WCa -- "$1"
        ;;

    ## jupyter notebooks
    application/json)
        if [ $ext == ipynb ]
        then
            jupyter nbconvert --to markdown "$1" --stdout \
                | highlight --syntax=markdown \
                            --out-format="xterm256" \
                            --replace-tabs="4" \
                            --style="$HOME/.config/highlight/rebecca_m.theme" \
                            --force
            #    | env COLORTERM=8bit bat --color=always --style=plain --language=markdown
        else
            highlight --out-format="xterm256" --replace-tabs="4" --style="$HOME/.config/highlight/rebecca_m.theme" --force -- "$1"
        fi
        ;;

    text/html)
        #lynx -width="$4" -display_charset=utf-8 -dump "$1"
        elinks -dump -dump-color-mode 2 "$1"
        ;;

    ## text | markdown
    # text/* | */xml | application/x-ndjson)
    #     if [ $ext == md ]
    #     then
    #         glow -s dark -w 0 "$1"
    #     else
    #         bat --style plain --theme ansi --terminal-width "$(($4-2))" -f "$1"
    #     fi
    #     ;;

    ## syntax highlight
    text/* | */xml | application/x-ndjson)
        ## Syntax highlight below 256KiB
        # if [[ "$( stat --printf='%s' -- "${path}" )" -gt "262143" ]]; then
        #     exit 1
        # fi
        ##themes: use highlight-gui
        ##nice themes: "base16/google-dark", "base16/materia", "navy", "$HOME/.config/highlight/rebecca_m.theme"
        highlight --out-format="xterm256" --replace-tabs="4" --style="$HOME/.config/highlight/rebecca_m.theme" --force -- "$1"
        ;;

    *)
        mediainfo "$1"
        echo "mime: $mime"
        ;;

esac

exit 1
