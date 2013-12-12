{
    module TestGram (tparse) where
}

%name tparse  
%tokentype    { String  }
%token one    { "1"     } 
       if     { "if"    }
       then   { "then"  }
       else   { "else"  }

%%

stmt: open
    | closed

open: if one then stmt             {"if 1 then ("++$4++")"}
    | if one then closed else open {"if 1 then ("++$4++") else ("++$6++")"}

closed: one                            {"1"}
      | if one then closed else closed {"if 1 then ("++$4++") else ("++$6++")"}


{
 happyError = error "parse error"
}
