# Check if user has bat installed. If not, use cat instead.
if command -v bat  >/dev/null 2>&1
then
    cat="bat -l lisp"
else
    cat=cat
fi

for demo in ./demos/*; do
    clear
    echo "Running demo $demo"
    $cat $demo
    echo "Press enter to continue"
    read
    dune exec ocaml_borrow_checker -- $demo
    echo "Press enter to continue"
    read
done
