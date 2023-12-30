#!/bin/bash

show_help() {
    echo "Usage: $0 -s|--source [url] -d|--destination [path] [-p|--probe]"
    echo "  -s | --source       URL to download from."
    echo "  -d | --destination  Directory path to save the downloaded files."
    echo "  -p | --probe        Optional: Download 10 random files if specified."
}

SOURCE=""
DESTINATION=""
PROBE=false

# Parse command-line options
if ! OPTIONS=$(getopt -o s:d:p --long source:,destination:,probe -- "$@"); then
    echo "Error: Failed parsing options." >&2
    exit 1
fi

eval set -- "$OPTIONS"

while true; do
    case "$1" in
    -s | --source)
        SOURCE="$2"
        shift 2
        ;;
    -d | --destination)
        DESTINATION="$2"
        shift 2
        ;;
    -p | --probe)
        PROBE=true
        shift
        ;;
    --)
        shift
        break
        ;;
    *)
        show_help
        exit 1
        ;;
    esac
done

# Validate input parameters
if [ -z "$SOURCE" ] || [ -z "$DESTINATION" ]; then
    show_help
    exit 1
fi

# Create destination directory if it does not exist
mkdir -p "$DESTINATION"

dl_spbu_s_e() {
  curl "$SOURCE" -s --compressed -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/118.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8' -H 'Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3' -H 'Accept-Encoding: gzip, deflate, br' -H 'DNT: 1' -H 'Connection: keep-alive'
}

dl_spbu_oop() {
    local result
    result=$(dl_spbu_s_e grep -o -E "'https://nc\.spbu\.ru/.+?'" | sed "s/'//g" | \
             grep -oP 'https://nc\.spbu\.ru/s/[^\s<>"'\'']+' | sort | uniq )
    if [ "$PROBE" = true ]; then
        echo "$result" | sort -R | head -n 10
    else
        echo "$result"
    fi
}

extract_and_cleanup() {
    local file_path="$1"
    local dest_dir="$2"

    unzip -o "$file_path" -d "$dest_dir"
    local status=$?
    if [ $status -ne 0 ]; then
        >&2 echo "Failed to extract $file_path"
        return $status
    fi

    if ! rm -f "$file_path"; then
        >&2 echo "Failed to remove the zip file $file_path"
        return 1
    fi
}


download() {
    local url="$1"
    # Generate a unique name from the URL by replacing '/' and ':' characters
    local filename="${url//[^a-zA-Z0-9]/_}"
    local filepath="$DESTINATION/$filename.zip"

    if ! wget -O "$filepath" "${url}/download"; then
        >&2 echo "Cannot download $url"
        return 15
    fi

    echo "$filepath"
}

update_recoll_index() {
    echo "Updating Recoll index..."
    recollindex -r "$DESTINATION"
    local status=$?
    if [ $status -ne 0 ]; then
        >&2 echo "Failed to update Recoll index."
        return $status
    else
        echo "Recoll index updated successfully."
    fi
}

for u in $(dl_spbu_oop); do
    file_url="${u}"

    # Download the file and receive the unique filepath back
    local_file=$(download "$file_url")
    download_status=$?
    if [ $download_status -ne 0 ]; then
        continue
    fi

    extract_and_cleanup "$local_file" "$DESTINATION"
    echo $?
done

update_recoll_index
update_status=$?
if [ $update_status -ne 0 ]; then
    exit $update_status
fi
