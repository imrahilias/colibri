#!/usr/bin/env bash

set -o noclobber -o noglob -o nounset -o pipefail
IFS=$'\n'

# If the option `use_preview_script` is set to `true`,
# then this script will be called and its output will be displayed in ranger.
# ANSI color codes are supported.
# STDIN is disabled, so interactive scripts won't work properly

# This script is considered a configuration file and must be updated manually.
# It will be left untouched if you upgrade ranger.

# Meanings of exit codes:
# code | meaning    | action of ranger
# -----+------------+-------------------------------------------
# 0    | success    | Display stdout as preview
# 1    | no preview | Display no preview at all
# 2    | plain text | Display the plain content of the file
# 3    | fix width  | Don't reload when width changes
# 4    | fix height | Don't reload when height changes
# 5    | fix both   | Don't ever reload
# 6    | image      | Display the image `$IMAGE_CACHE_PATH` points to as an image preview
# 7    | image      | Display the file directly as an image

# Script arguments
FILE_PATH="${1}"         # Full path of the highlighted file
PV_WIDTH="${2}"          # Width of the preview pane (number of fitting characters)
PV_HEIGHT="${3}"         # Height of the preview pane (number of fitting characters)
IMAGE_CACHE_PATH="${4}"  # Full path that should be used to cache image preview
PV_IMAGE_ENABLED="${5}"  # 'True' if image previews are enabled, 'False' otherwise.

FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER=$(echo ${FILE_EXTENSION} | tr '[:upper:]' '[:lower:]')

# Settings
HIGHLIGHT_SIZE_MAX=262143  # 256KiB
HIGHLIGHT_TABWIDTH=8
HIGHLIGHT_STYLE='base16/google-dark' #'moria' #'pablo'
PYGMENTIZE_STYLE='autumn'


handle_extension() {
    case "${FILE_EXTENSION_LOWER}" in
        # Archive
        a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|\
        rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip)
            atool --list -- "${FILE_PATH}" && exit 5
            bsdtar --list --file "${FILE_PATH}" && exit 5
            exit 1;;
        rar)
            # Avoid password prompt by providing empty password
            unrar lt -p- -- "${FILE_PATH}" && exit 5
            exit 1;;
        7z)
            # Avoid password prompt by providing empty password
            7z l -p -- "${FILE_PATH}" && exit 5
            exit 1;;

        # PDF
        pdf)
            # Preview as text conversion
            pdftotext -l 10 -nopgbrk -q -- "${FILE_PATH}" - | fmt -w ${PV_WIDTH} && exit 5
            mutool draw -F txt -i -- "${FILE_PATH}" 1-10 | fmt -w ${PV_WIDTH} && exit 5
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
        
        # BitTorrent
        torrent)
            transmission-show -- "${FILE_PATH}" && exit 5
            exit 1;;

        # OpenDocument
        odt|ods|odp|sxw)
            # Preview as text conversion
            odt2txt "${FILE_PATH}" && exit 5
            ## Preview as markdown conversion
            #pandoc -s -t markdown -- "${FILE_PATH}" && exit 5
            exit 1;;
        
        ## XLSX
        xlsx)
            ## Preview as csv conversion
            ## Uses: https://github.com/dilshod/xlsx2csv
            xlsx2csv -- "${FILE_PATH}" && exit 5
            exit 1;;
        
        # Markdown
        md)
            highlight --syntax=markdown --out-format=ansi "${FILE_PATH}" && exit 5
            # glow -s dark "${FILE_PATH}" && exit 5
            # bat "${FILE_PATH}" && exit 5
            exit 1;;
        
        # HTML
        htm|html|xhtml)
            # Preview as text conversion
            w3m -dump "${FILE_PATH}" && exit 5
            lynx -dump -- "${FILE_PATH}" && exit 5
            elinks -dump "${FILE_PATH}" && exit 5
            pandoc -s -t markdown -- "${FILE_PATH}" && exit 5
            ;; # Continue with next handler on failure

        ## JSON
        json|ipynb)
            jq --color-output . "${FILE_PATH}" && exit 5
            python -m json.tool -- "${FILE_PATH}" && exit 5
            ;;
    esac
}

image() {
	if [ -f "$1" ] && [ -n "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ] && command -V ueberzug >/dev/null 2>&1; then
		printf '{"action": "add", "identifier": "PREVIEW", "x": "%s", "y": "%s", "width": "%s", "height": "%s", "scaler": "contain", "path": "%s"}\n' "$4" "$5" "$(($2-1))" "$(($3-1))" "$1" > "$FIFO_UEBERZUG"
	else
		mediainfo "$6"
	fi
}


handle_image() {
    local mimetype="${1}"
    case "${mimetype}" in
        image/avif)
            CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/lf/thumb.$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | cut -d' ' -f1)"
	    [ ! -f "$CACHE" ] && convert "$1" "$CACHE.jpg"
	    image "$CACHE.jpg" "$2" "$3" "$4" "$5" "$1" ;;
	image/vnd.djvu)
	    CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/lf/thumb.$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | cut -d' ' -f1)"
	    [ ! -f "$CACHE" ] && djvused "$1" -e 'select 1; save-page-with /dev/stdout' | convert -density 200 - "$CACHE.jpg" > /dev/null 2>&1
	    image "$CACHE.jpg" "$2" "$3" "$4" "$5" "$1" ;;
        image/svg+xml)
	    CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/lf/thumb.$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | cut -d' ' -f1)"
	    [ ! -f "$CACHE" ] && inkscape --convert-dpi-method=none -o "$CACHE.png" --export-overwrite -D --export-png-color-mode=RGBA_16 "$1"
	    image "$CACHE.png" "$2" "$3" "$4" "$5" "$1"
	    ;;
        image/*) image "$1" "$2" "$3" "$4" "$5" "$1" ;;
        #     ;;
    esac
}

handle_mime() {
    local mimetype="${1}"
    case "${mimetype}" in
        
        ## RTF and DOC
        text/rtf|*msword)
            ## Preview as text conversion
            ## note: catdoc does not always work for .doc files
            ## catdoc: http://www.wagner.pp.ru/~vitus/software/catdoc/
            catdoc -- "${FILE_PATH}" && exit 5
            exit 1;;

        ## DOCX, ePub, FB2 (using markdown)
        ## You might want to remove "|epub" and/or "|fb2" below if you have
        ## uncommented other methods to preview those formats
        *wordprocessingml.document|*/epub+zip|*/x-fictionbook+xml)
            ## Preview as markdown conversion
            pandoc -s -t markdown -- "${FILE_PATH}" && exit 5
            exit 1;;
        
        ## XLS
        *ms-excel)
            ## Preview as csv conversion
            ## xls2csv comes with catdoc:
            ##   http://www.wagner.pp.ru/~vitus/software/catdoc/
            xls2csv -- "${FILE_PATH}" && exit 5
            exit 1;;
        
        ## Text
        text/* | */xml)
            ## Syntax highlight
            if [[ "$( stat --printf='%s' -- "${FILE_PATH}" )" -gt "${HIGHLIGHT_SIZE_MAX}" ]]; then
                exit 2
            fi
            if [[ "$( tput colors )" -ge 256 ]]; then
                local pygmentize_format='terminal256'
                local highlight_format='xterm256'
            else
                local pygmentize_format='terminal'
                local highlight_format='ansi'
            fi
            #env COLORTERM=8bit env TERM=screen bat --decorations=always --color=always --style="plain" "${FILE_PATH}" && exit 5
            env TERM=screen-256color highlight --replace-tabs="${HIGHLIGHT_TABWIDTH}" --out-format="${highlight_format}" \
                      --style="${HIGHLIGHT_STYLE}" --force -- "${FILE_PATH}" && exit 5
            pygmentize -f "${pygmentize_format}" -O "style=${PYGMENTIZE_STYLE}" -- "${FILE_PATH}" && exit 5
            exit 2;;
        
        # ## DjVu
        # image/vnd.djvu)
        #     ## Preview as text conversion (requires djvulibre)
        #     djvutxt "${FILE_PATH}" | fmt -w "${PV_WIDTH}" && exit 5
        #     exiftool "${FILE_PATH}" && exit 5
        #     exit 1;;

        ## Image
        image/*)
            # Preview as text conversion
            #img2txt --gamma=0.6 --width="${PV_WIDTH}" -- "${FILE_PATH}" && exit 4
            #img2txt -f troff --width="${PV_WIDTH}" -- "${FILE_PATH}" && exit 4
            img2txt --width="${PV_WIDTH}" -- "${FILE_PATH}" && exit 4
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
        
        ## Video and audio
        video/* | audio/*)
            mediainfo "${FILE_PATH}" && exit 5
            exiftool "${FILE_PATH}" && exit 5
            exit 1;;
    esac
}

handle_fallback() {
    echo '----- File Type Classification -----' && file --dereference --brief -- "${FILE_PATH}" && exit 5
    exit 1
}


MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"
handle_image "${MIMETYPE}"
handle_extension
handle_mime "${MIMETYPE}"
handle_fallback

exit 1
