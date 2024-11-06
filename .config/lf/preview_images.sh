#!/bin/sh

# file preview handler for lf with image preview via uberzug.

set -C -f
IFS="$(printf '%b_' '\n')"; IFS="${IFS%_}"

image() {
    if [ -f "$1" ] && [ -n "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ] && command -V ueberzug >/dev/null 2>&1; then
	printf '{"action": "add", "identifier": "PREVIEW", "x": "%s", "y": "%s", "width": "%s", "height": "%s", "scaler": "contain", "path": "%s"}\n' "$4" "$5" "$(($2-1))" "$(($3-1))" "$1" > "$FIFO_UEBERZUG"
    else
	exiv2 "$6"
    fi
}

highlighter="bat"
highlightz() {
    if [[ $highlighter == "bat" ]]; then
         # Syntax highlight with bat (see `bat --list-themes`):
        bat --style plain --theme "OneHalfDark" --terminal-width "$(($4-2))" -f "$1"

        # Syntax highlight with highlight below 256KiB:
        if [[ "$( stat --printf='%s' -- "${path}" )" -gt "262143" ]]; then
            exit 1
        fi

    else
        # Syntax highlight with highlight (see `highlight-gui`):
        highlight --out-format="xterm256" \
                  --replace-tabs="4" \
                  --style="$HOME/.config/highlight/rebecca_m.theme" \
                  --max-size 256K \
                  --force "$1"
    fi
}


# note that the cache file name is a function of file information, meaning if
# an image appears in multiple places across the machine, it will not have to
# be regenerated once seen.

path="$1"
file="${path##*/}"
mime="$(file --dereference --brief --mime-type -- "$1")"
name="${file%%.*}" # shell escape
ext="${file#*.}" # shell escape; "example.tar.gz" gives "tar.gz"
cache="$HOME/.thumbnail/$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | cut -d' ' -f1)"

case $mime in

    # # debug
    #  *)
    #      echo $mime
    #      ;;

    # very fast but jpeg only via libjpg:
    image/jpeg)
        [ ! -f "$cache" ] && epeg --width=1024 --preserve "$1" "$cache"
        image "$cache" "$2" "$3" "$4" "$5" "$1"
        ;;

    image/svg+xml)
        # [ ! -f "$cache" ] && inkscape --convert-dpi-method=none \
            #                               -o "$cache.png" \
            #                               --export-overwrite -D \
            #                               --export-png-color-mode=RGBA_16 "$1"
        [ ! -f "$cache" ] && rsvg-convert --keep-aspect-ratio --width=1024 -o "$cache" "$1"
        image "$cache" "$2" "$3" "$4" "$5" "$1"
        ;;

    image/*)
        [ ! -f "$cache" ] && convert -thumbnail 1024 "$1" "$cache"
        image "$cache" "$2" "$3" "$4" "$5" "$1"
        ;;

    # xls | xlsx | ods ... whatever libreoffice can open!
    application/vnd*)
        [ ! -f "$cache" ] && libreoffice --headless \
                                         --invisible \
                                         --convert-to jpg \
                                         --outdir "/tmp" \
                                         "$1" >/dev/null && \
            mv "/tmp/$name.jpg" "$cache"
        image "$cache" "$2" "$3" "$4" "$5" "$1"
        ;;

    # epub, mobi, fb2 (using calibre)
    application/epub+zip|application/x-mobipocket-ebook|\
        application/x-fictionbook+xml)
        [ ! -f "$cache" ] && ebook-meta --get-cover="$cache" -- "$1"
	image "$cache" "$2" "$3" "$4" "$5" "$1"
	;;

    video/* )
        [ ! -f "$cache" ] && ffmpegthumbnailer -i "$1" -o "$cache" -s 0
        image "$cache" "$2" "$3" "$4" "$5" "$1"
        ;;

    application/pdf)
        [ ! -f "$cache.jpg" ] && pdftoppm -f 1 -l 1 \
                                          -scale-to-x 1024 \
                                          -scale-to-y -1 \
                                          -singlefile \
                                          -jpeg -tiffcompression jpeg \
                                          -- "$1" "$cache"
	image "$cache.jpg" "$2" "$3" "$4" "$5" "$1"
	;;

    application/*zip)
        atool --list -- "$1"
        ;;

    application/pgp-encrypted)
        gpg -d -- "$1"
        ;;

    # executables and shared objects)
    application/x-executable | application/x-pie-executable | application/x-sharedlib)
        readelf -WCa -- "$1"
        ;;

    # executables subtitles
    application/x-subrip)
        highlightz "$1"
        ;;

    # jupyter notebooks
    application/json)

        case $ext in

            ipynb)

                if [[ $highlighter == "bat" ]]; then
                    jupyter nbconvert --to markdown "$1" --stdout | highlightz --language=markdown
                else
                    jupyter nbconvert --to markdown "$1" --stdout | highlightz --syntax=markdown
                fi
                ;;

            *) highlightz "$1"
               ;;
        esac
        ;;

    text/html)
        #lynx -width="$4" -display_charset=utf-8 -dump "$1"
        elinks -dump -dump-color-mode 2 "$1"
        ;;

    # text syntax highlight:
    text/* | */xml | application/x-ndjson | application/javascript )

        # almighty highlight:
        highlightz "$1"
        ;;

    *)
        mediainfo "$1"
        echo "mime: $mime"
        ;;

esac

exit 1
