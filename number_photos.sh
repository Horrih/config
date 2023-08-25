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
    echo './renommage_date.sh "/mnt/c/Users/Charles/Desktop/A trier/2018" ~/PhotosNAS/Photos/2018'
    echo ""
    echo "Options:"
    echo "    --move, -m : On déplace le fichier existant au lieu de copier. Plus rapide mais plus risqué"
    echo "    --dry-run, -d : Execution en mode simulation, la copie ne sera pas effectuée"
    echo "    --help, -h : Ce message d'aide"
    echo ""
}

dry_run=false
move=false
for arg in "$@"; do
    case "$arg" in
        -h | --help) print_help; exit 0;;
        --dry-run | -d) dry_run=true;;
        --move | -m) move=true;;
        *)
            if [[ -z "$LOCAL" ]]; then
                LOCAL="$arg";
            elif [[ -z "$NAS" ]]; then
                NAS="$arg"
            else
                echo "Argument en trop : $arg"
                exit 1
            fi
            ;;
    esac
done

if [[ -z "$LOCAL" ]] || [[ -z "$NAS" ]]; then
    echo "Erreur : usage incorrect"
    print_help
    exit 1
fi

echo "Lancement du script de renommage des dates:"
echo "  - Dossier de départ : $LOCAL"
echo "  - Dossier destination : $NAS"
if [ "$dry_run" = true ]; then
    echo "Options de lancement de simulation : les opérations seront fictives"
fi
if [ "$move" = true ]; then
    echo "Les fichiers seront déplacés et non copiés"
fi
total=0

#To disable the splitting of files with a whitespace in the name
IFS=$(echo -en "\n\b")

#for f in $(cd "$LOCAL" && find . -maxdepth 1 -type f | sed 's|./||' ); do
for f in $(cd "$LOCAL" && ls -rtp | grep -v '/' | grep -v 'Thumbs.db' ); do
    ((total++))
    date=$(stat -c %y "$LOCAL/$f" | cut -d ' ' -f 1) # Uncomment to remove year | sed 's/[0-9]*-//')
    #already_present=false
    #for f2 in "$NAS/"$date*; do test -f "$f2" && cmp -s "$f2" "$LOCAL/$f" && already_present=true; done
    #if [ "$already_present" = true ]; then
    #    echo "Source=$LOCAL/$f identique à $NAS/$f2 : il sera ignoré"
    #    continue
    #fi
    i=1
    while : ; do
	extension="${f##*.}"
	destfile="$date ($i).$extension"
	naspath="$NAS/$destfile"
	printf "Source=$LOCAL/$f -> $naspath"
	if [[ -f $naspath ]]; then
	    echo " | Déjà pris";
	else
	    echo " | OK! | Fichier n°$total"
            if [ "$dry_run" = false ]; then
                if [ "$move" = true ]; then
                    mv "$LOCAL/$f" "$naspath"
                else
	            cp -a "$LOCAL/$f" "$naspath"
                fi
            fi
	    break
	fi
	((i++))
    done
done
echo ""
echo "Terminé : copie de $total fichiers"
