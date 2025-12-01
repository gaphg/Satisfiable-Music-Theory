        /* File parser.mly */
        %token TEST_TOKEN
        %token EOL
        %start main
        %type <string> main
        %%
        main:
            expr EOL expr EOL        { $1 ^ $3 }
        ;
        expr:
            TEST_TOKEN   { "test tokens!" }
        ;
