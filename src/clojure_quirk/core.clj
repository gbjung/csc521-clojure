(ns clojure_quirk.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [clojure_quirk.core]
            [clojure.string :as cstr]))

(let [m (.getDeclaredMethod clojure.lang.LispReader
                            "matchNumber"
                            (into-array [String]))]
  (.setAccessible m true)
  (defn parse-number [s]
    "Uses LISP's matchNumber method to return the correct number
    type (int, double, float) when parse-number is called"
    (.invoke m clojure.lang.LispReader (into-array [s]))))


(def store
  "A Global scope used for holding variables and functions"
  (atom {}))


(def mapa
  "Used to map index value with a variable or function name"
  (atom {}))

(def counter
  "Counts every time a function is called"
  (atom 0))

(def varcounter
  "Counts how many variables are declared in a function"
  (atom 0))

(def returncounter
  "Counts how many return variable are in a function"
  (atom 0))

(defn CallByLabel [label & args]
  "Calls the corresponding function based on the string name"
  (apply (ns-resolve 'clojure_quirk.core (symbol(name label))) args))

(defn interpret-quirk [subtree]
  "Checks to see the Parsed tokens are correct initially"
  (if (= (first subtree) :Program)
    (CallByLabel "Program" subtree)
    (println "Error at Program level: " subtree)))

(defn Program [subtree]
  "Method for Program Level:
  Statement Program | Statement"
  (if (= (count subtree) 2)
    (CallByLabel "Statement" (list (nth (nth subtree 1) 1)))
    (do
      (CallByLabel "Statement"  (list (nth (nth subtree 1) 1)))
      (CallByLabel "Program" (nth subtree 2)))))

(defn Statement [subtree]
  "Method for Statement level
  FunctionDeclaration | Assignment | Print"
  (case (first (first subtree))
    :Print, (CallByLabel "Print" (rest (first subtree)))
    :Assignment (CallByLabel "Assignmenta" (rest (first subtree)))
    :FunctionDeclaration (CallByLabel "FunctionDeclaration" (rest (first subtree)))
    (println "Error at Statement level: " (first (first subtree)))))


(defn Assignmenta [subtree]
  "Method for Assignment
   SingleAssignment | MultipleAssignment"
  (CallByLabel "SingleAssignment" (rest (first subtree))))

(defn Print [subtree]
  "Method for Print
  PRINT Expression"
  (println (double (eval (read-string (str "(+ " (CallByLabel "Expression" (rest (first (rest subtree))))")"))))))

(defn FunctionDeclaration [subtree]
  "Method for FunctionDeclaration
  FUNCTION Name LPAREN FunctionParams LBRACE FunctionBody RBRACE
  Uses local variables to construct a valid clojure function
  using string evaluation to store the function in memory.
  Function parameters and the function name itself
  are stored in global Store."
  (let [funcname (CallByLabel "Namea" (nth subtree 1))
        vars (CallByLabel "FunctionParams" (nth subtree 3))
        functionbody (CallByLabel "FunctionBody" (nth subtree 5))
        returns (cstr/split functionbody #",")
        ]
    (swap! store assoc :vars vars)
    (if (> (count returns) 2)
      (doseq [x returns]
        (swap! store assoc (keyword (str funcname @returncounter)) (str "(defn " funcname "[" vars "] "  x ")"))
        (swap! returncounter inc))
      (if (= vars "None")
        (eval (read-string (str "(defn " funcname  " [] " functionbody")")))
        (eval (read-string (str "(defn " funcname "[" vars "] "  functionbody")")))))))

(defn FunctionParams [subtree]
  "Method for FunctionParams
   NameList RPAREN | RPAREN"
  (if (= (first (first (rest subtree))) :RPAREN)
    (str "None")
    (CallByLabel "NameList" (nth subtree 1))))

(defn FunctionBody [subtree]
  "Method for FunctionBody
  Program Return | Return"
  (if (= (count subtree) 2)
    (CallByLabel "Return" (nth subtree 1))
    (do
      (str (CallByLabel "Program" (nth subtree 1))
           (CallByLabel "Return" (nth subtree 2))))))

(defn Return [subtree]
  "Method for Return
  RETURN ParameterList"
  (CallByLabel "ParameterList" (nth subtree 2)))


(defn SingleAssignment [subtree]
  "Method for SingleAssignment and MultipleAssignment
  VAR Name ASSIGN Expression
  Checks to see if the Assignment is a functioncall, then maps
  the variable along with a string representation of the function in the global Store.
  It repeats this for a MultipleAssignment to assign every variable with the corresponding return.
  For SingleAssignment, it maps the variable name with the counter value.
  This is used to retrieve specific function returns via colon.
  It maps the variable name with the returned expression value to the global Store."
  (if (=(first (nth subtree 3)) :FunctionCall)
    (if (=(count (nth subtree 1))2)
      (do
        (swap! counter inc)
        (swap! store assoc (keyword (CallByLabel "NameList" (nth subtree 1)))
               (str "((fn ["
                    (get (deref store) :vars) "]"
                    (get (deref store) (get (deref mapa) @counter)) ")"
                    (CallByLabel "ParameterList" (nth (nth (rest (last subtree)) 2) 1)) ")")))
      (doseq [x (rest (nth subtree 1))]
        (let [op (case (first x)
                   :Name (CallByLabel "Namea" x)
                   :NameList (CallByLabel "NameList" x)
                   nil)]
          (if (= op nil)
            (println " ")
            (do
              (swap! counter inc)
              (swap! store assoc (keyword op)
                     (str "((fn [" (get (deref store) :vars) "]"
                          (get (deref store) (get (deref mapa) @counter)) ")"
                          (CallByLabel "ParameterList" (nth (nth (rest (last subtree)) 2) 1)) ")")))))))
    (do
      (swap! varcounter inc)
      (swap! mapa assoc @varcounter (keyword (CallByLabel "Namea" (nth subtree 1))))
      (swap! store assoc (keyword (CallByLabel "Namea" (nth subtree 1))) (CallByLabel "Expression" (rest (last subtree)))))))

(defn ParameterList [subtree]
  "Method for ParameterList
  Parameter COMMA ParameterList | Parameter
  "
  (if (=(count subtree)2)
    (str (CallByLabel "Parameter" (nth subtree 1)))
    (str (CallByLabel "Parameter" (nth subtree 1)) "," (CallByLabel "ParameterList" (nth subtree 3)))))

(defn Parameter [subtree]
  "Method for Parameter
  Expression | Name
  "
  (case (first(nth subtree 1))
    :Expression, (CallByLabel "Expression" (list (nth (nth subtree 1) 1)))
    :Name, (CallByLabel "Namea" (nth subtree 1))
    (println "Fucking Parameter: " subtree)))

(defn NameList [subtree]
  "Method for NameList
  Name COMMA NameList | Name"
  (if (=(count subtree) 2)
    (CallByLabel "Namea" (nth subtree 1))
    (str
      (CallByLabel "Namea" (nth subtree 1)) " "
      (CallByLabel "NameList" (nth subtree 3)))))

(defn Expression [subtree]
  "Method for Expression
  Term ADD Expression | Term SUB Expression | Term"
  (if (=(count subtree) 1)
    (CallByLabel "Term" (rest (first subtree)))
    (let [subadd (nth (nth subtree 1)1)
          term  (CallByLabel "Term" (list (first (rest (first subtree)))))
          expression (CallByLabel "Expression" (rest (last subtree)))]
      (str "("subadd " "term " " expression ")"))))

(defn Term [subtree]
  "Method for Term
  Factor MULT Term | Factor DIV Term | Factor"
  (if (=(count subtree) 1)
    (CallByLabel "Factor" (rest (first subtree)))
    (let [multdiv (str (nth (nth subtree 1)1))
          factor (CallByLabel "Factor" (rest (first subtree)))
          term (CallByLabel "Term" (rest (last subtree)))]
      (str "("multdiv " "factor " " term ")"))))

(defn Factor [subtree]
  "Method for Factor
  SubExpression EXP Factor | SubExpression | FunctionCall | Value EXP Factor | Value
  Depending on the size of the subtree and the value of the first keyword,
  it runs the matching function call. The Expression, it generates a valid
  clojure string that would equal Factor^Value."
  (if (= (count subtree) 1)
    (case (first (first subtree))
      :Value,  (CallByLabel "Value" (rest (first subtree)))
      :FunctionCall (CallByLabel "FunctionCall" (rest (first subtree)))
      :SubExpression, (CallByLabel "SubExpression" (rest (rest (first subtree)))))
    (if (= (first (nth subtree 1)) :EXP)
      (str "(reduce * (repeat "
           (CallByLabel "Factor" (rest (nth subtree 2)))
           " "
           (CallByLabel "Value" (rest (nth subtree 0))) "))")
      (do
        (case (first (first subtree))
          :SubExpression, (CallByLabel "SubExpression" (rest (rest (first subtree))))
          :Value, (CallByLabel "Value" (list (first subtree)))
          :FunctionCall, (CallByLabel "FunctionCall" (rest (rest (first subtree))))
          (println "Failed: " (first (first subtree))))))))

(defn FunctionCall [subtree]
  "Method for FunctionCall
  Name LPAREN FunctionCallParams COLON Number | Name LPAREN FunctionCallParams
  If COLON Number, it indexes the proper return variable from the
  Global store and runs that through string evaluation of an anonymous function."
  (if (= (count subtree) 3)
    (let [funcname (CallByLabel "Namea" (first subtree))]
      (if (= (count (nth subtree 2)) 2)
        (str "(" funcname ")")
        (let [params (CallByLabel "ParameterList" (nth (nth subtree 2) 1))]
          (str "(" funcname " " params ")"))))
    (let [funcname (CallByLabel "Namea" (first subtree))
          returnindex (CallByLabel "Numbera" (list (last subtree)))]
      (eval (read-string (get (deref store) (keyword (str funcname returnindex)))))
      (CallByLabel "FunctionCall" (take 3 subtree)))))

(defn SubExpression [subtree]
  "Method for SubExpression
  LPAREN Expression RPAREN"
  (CallByLabel "Expression" (rest (first subtree))))

(defn Value [subtree]
  "Method for Value
  Name | Number"
  (case (first (first subtree))
    :Number, (CallByLabel "Numbera" subtree)
    :Name, (CallByLabel "Namea" (first subtree))
    (str "Failed at Value")))

(defn Numbera [subtree]
  "Method for Number
  NUMBER | SUB NUMBER | ADD NUMBER
  If SUB or ADD, it returns a string representation of
  the difference of 0 and that value to return a positive
  or negative number"
  (if (= (count (first subtree)) 2)
    (parse-number (nth (nth (first subtree) 1) 1))
    (let [addsub (nth (nth (first subtree) 1) 1)]
      (str "(" addsub " 0 " (nth (nth (first subtree) 2) 1) ")"))))

(defn Namea [subtree]
  "Method for Name
  IDENT | SUB IDENT | ADD IDENT
  If the IDENT exists as a currently binded variable,
  it returns that value from the global store. If not,
  it returns the string value itself.
  If SUB or ADD, it returns a string representation of
  the difference of 0 and that value to return a positive
  or negative number
  "
  (if (= (count subtree) 3)
    (let [addsub (nth (nth subtree 1) 1)]
      (try
        (if (= (get (deref store) (keyword (str (nth (nth subtree 2) 1)))) nil)
          (str "(" addsub " 0 " (nth (nth subtree 2) 1) " )")
          (str "(" addsub " 0 "(get (deref store) (keyword (str (nth (nth subtree 2) 1)))) " )"))
        (catch Exception e (str "caught exception: " subtree (.getMessage e)))))
    (try
      (if (= (get (deref store) (keyword (str (nth (nth subtree 1) 1)))) nil)
        (str (nth (nth subtree 1) 1))
        (str (get (deref store) (keyword (str (nth (nth subtree 1) 1))))))
      (catch Exception e (str "caught exception: " subtree (.getMessage e))))))


(defn -main [& args]
  (if (.equals "-pt" (first *command-line-args*))
    (def SHOW_PARSE_TREE true))
  (def quirk-parser (insta/parser (slurp "resources/quirk-grammar-ebnf.txt") :auto-whitespace :standard))
  (def parse-tree (quirk-parser (slurp *in*)))
  (if (= true SHOW_PARSE_TREE)
    (do
      (println parse-tree)
      (interpret-quirk parse-tree))
    (interpret-quirk parse-tree)))