#! /bin/bash

function remove_duplicates()
{
    for f in $(/bin/ls);
    do
        sha1sum $f >> sha1.txt
    done
    for sha1 in $(cat sha1.txt | sed 's/ .*//'  | sort | uniq -c | grep -v "1 " | sed 's/ *. *//');
    do
        echo "Removing duplicated sha1 $sha1";
        for f in $(grep $sha1 sha1.txt | sed 's/.*  //' | tail -n +2 );
        do
            rm -v $f
        done
    done
    rm sha1.txt
}

function print_help()
{
    echo "Usage $0  DOSSIER_LOCAL   DOSSIER_NAS"
    echo ""
    echo "exemple:"
    echo './renommage_date.sh "DOSSIER_A_TRIER'
    echo ""
    echo "Options:"
    echo "    --dry-run, -d : Execution en mode simulation, la copie ne sera pas effectuée"
    echo "    --help, -h : Ce message d'aide"
    echo ""
}

dry_run=false
for arg in "$@"; do
    case "$arg" in
        -h | --help) print_help; exit 0;;
        --dry-run | -d) dry_run=true;;
        *)
            if [[ -z "$DIR" ]]; then
                DIR="$arg";
            else
                echo "Argument en trop : $arg"
                exit 1
            fi
            ;;
    esac
done
if [[ ! -d "$DIR" ]]; then
    echo "Erreur : usage incorrect ou chemins invalides"
    print_help
    exit 1
fi

echo "Lancement du script de renommage des dates:"
echo "  - Dossier à trier : $DIR"
if [ "$dry_run" = true ]; then
    echo "Options de lancement de simulation : les opérations seront fictives"
fi
total=0

#To disable the splitting of files with a whitespace in the name
IFS=$(echo -en "\n\b")

TMP="$DIR/temp_tri"
if [ "$dry_run" = false ]; then
    mkdir -pv "$TMP"
fi
last_date=""
for f in $(find $DIR -type f -printf "%T@|%p\n" | sort -u | sed 's/.*|//g'); do
    ((total++))
    date=$(stat -c %y "$f" | cut -d ' ' -f 1) # Uncomment to remove year | sed 's/[0-9]*-//')
    if [[ "$last_date" != "$date" ]]; then
        i=1
    fi
    last_date=$date
    extension="${f##*.}"
    destfile="$date ($i).$extension"
    destpath="$TMP/$destfile"
    echo "Source=$DIR/$f -> $destpath -> $DIR/$destfile"
    if [ "$dry_run" = false ]; then
        mv "$f" "$destpath"
    fi
    ((i++))
done
if [ "$dry_run" = false ]; then
    echo ""
    echo "Rappatriement de $TMP vers $DIR"
    mv -v "$TMP"/* "$DIR"
fi
test -d $TMP && rmdir $TMP
echo ""
echo "Terminé : renommage de $total fichiers"
