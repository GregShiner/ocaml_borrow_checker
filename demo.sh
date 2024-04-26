for demo in `ls -v demos/`; do
    clear
    echo "Running demo $demo"
    bat -l lisp demos/$demo
    echo "Press enter to continue"
    read
    dune exec ocaml_borrow_checker -- demos/$demo
    echo "Press enter to continue"
    read
done
