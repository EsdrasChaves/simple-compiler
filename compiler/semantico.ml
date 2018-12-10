module Amb = Ambiente
module A = Ast
module S = Sast
module T = Tast

let rec posicao exp = let open S in
  match exp with
  | ExpVar v -> (match v with
      | A.VarSimples (_,pos) -> pos
      | A.VarCampo (_, (_,pos)) -> pos
    )
  | ExpInt (_,pos) -> pos
  | ExpReal (_, pos) -> pos
  | ExpString  (_,pos) -> pos
  | ExpBool (_,pos) -> pos
  | ExpOp ((_,pos),_,_)  -> pos
  | ExpChamadaF ((_,pos), _) -> pos

type classe_op = Aritmetico | Relacional | Logico | Cadeia 

let classifica op =
  let open A in
  match op with
    Or   -> Logico
  | And  -> Logico
  | Menor -> Relacional
  | Maior -> Relacional
  | MenorIgual -> Relacional
  | MaiorIgual -> Relacional
  | Igual -> Relacional
  | Difer -> Relacional
  | Mais -> Aritmetico
  | Menos -> Aritmetico
  | Mult -> Aritmetico
  | Div -> Aritmetico
  | Mod -> Aritmetico

let msg_erro_pos pos msg =
  let open Lexing in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  Printf.sprintf "Semantico -> linha %d, coluna %d: %s" lin col msg

let msg_erro nome msg =
  let pos = snd nome in
  msg_erro_pos pos msg

let nome_tipo t =
  let open A in
    match t with
      TypeInteger -> "inteiro"
    | TypeReal -> "real"
    | TypeString -> "string"
    | TypeBoolean -> "bool"
    | TipoVoid -> "void"

let mesmo_tipo pos msg tinf tdec =
  if tinf <> tdec
  then
    let msg = Printf.sprintf msg (nome_tipo tinf) (nome_tipo tdec) in
    failwith (msg_erro_pos pos msg)

let rec infere_exp amb exp =
  match exp with
    S.ExpInt n    -> (T.ExpInt (fst n, A.TypeInteger),       A.TypeInteger)
  | S.ExpReal r   -> (T.ExpReal (fst r, A.TypeReal),       A.TypeReal)
  | S.ExpString s -> (T.ExpString (fst s, A.TypeString), A.TypeString)
  | S.ExpBool b   -> (T.ExpBool (fst b, A.TypeBoolean),     A.TypeBoolean)
  | S.ExpVar v ->
    (match v with
       A.VarSimples nome ->
       (* Tenta encontrar a definição da variável no escopo local, se não      *)
       (* encontar tenta novamente no escopo que engloba o atual. Prossegue-se *)
       (* assim até encontrar a definição em algum escopo englobante ou até    *)
       (* encontrar o escopo global. Se em algum lugar for encontrado,         *)
       (* devolve-se a definição. Em caso contrário, devolve uma exceção       *)
       let id = fst nome in
         (try (match (Amb.busca amb id) with
               | Amb.EntVar tipo -> (T.ExpVar (A.VarSimples nome, tipo), tipo)
               | Amb.EntFun _ ->
                 let msg = "nome de funcao usado como nome de variavel: " ^ id in
                  failwith (msg_erro nome msg)
             )
          with Not_found ->
                 let msg = "A variavel " ^ id ^ " nao foi declarada" in
                 failwith (msg_erro nome msg)
         )
     | _ -> failwith "infere_exp: não implementado"
    )

  | S.ExpOp (op, esq, dir) ->
    let (esq, tesq) = infere_exp amb esq
    and (dir, tdir) = infere_exp amb dir in

    let verifica_aritmetico () =
      (match tesq with
         A.TypeInteger
       | A.TypeReal ->
         let _ = mesmo_tipo (snd op)
                      "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
                      tesq tdir
         in tesq (* O tipo da expressão aritmética como um todo *)

       | t -> let msg = "um operador aritmetico nao pode ser usado com o tipo " ^
                        (nome_tipo t)
         in failwith (msg_erro_pos (snd op) msg)
      )

    and verifica_relacional () =
      (match tesq with
         A.TypeInteger
       | A.TypeReal
       | A.TypeString ->
         let _ = mesmo_tipo (snd op)
                   "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
                   tesq tdir
         in A.TypeBoolean (* O tipo da expressão relacional é sempre booleano *)

       | t -> let msg = "um operador relacional nao pode ser usado com o tipo " ^
                        (nome_tipo t)
         in failwith (msg_erro_pos (snd op) msg)
      )

    and verifica_logico () =
      (match tesq with
         A.TypeBoolean ->
         let _ = mesmo_tipo (snd op)
                   "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
                   tesq tdir
         in A.TypeBoolean (* O tipo da expressão lógica é sempre booleano *)

       | t -> let msg = "um operador logico nao pode ser usado com o tipo " ^
                        (nome_tipo t)
              in failwith (msg_erro_pos (snd op) msg)
      )
    and verifica_cadeia () =
      (match tesq with
         A.TypeString ->
         let _ = mesmo_tipo (snd op)
                   "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
                   tesq tdir
         in A.TypeString (* O tipo da expressão relacional é sempre string *)

       | t -> let msg = "um operador relacional nao pode ser usado com o tipo " ^
                        (nome_tipo t)
              in failwith (msg_erro_pos (snd op) msg)
      )

    in
    let op = fst op in
    let tinf = (match (classifica op) with
          Aritmetico -> verifica_aritmetico ()
        | Relacional -> verifica_relacional ()
        | Logico -> verifica_logico ()
        | Cadeia -> verifica_cadeia ()
      )
    in
      (T.ExpOp ((op,tinf), (esq, tesq), (dir, tdir)), tinf)

  | S.ExpChamadaF (nome, args) ->
     let rec verifica_parametros ags ps fs =
        match (ags, ps, fs) with
         (a::ags), (p::ps), (f::fs) ->
            let _ = mesmo_tipo (posicao a)
                     "O parametro eh do tipo %s mas deveria ser do tipo %s" p f
            in verifica_parametros ags ps fs
       | [], [], [] -> ()
       | _ -> failwith (msg_erro nome "Numero incorreto de parametros")
     in
     let id = fst nome in
     try
       begin
         let open Amb in

         match (Amb.busca amb id) with
         (* verifica se 'nome' está associada a uma função *)
           Amb.EntFun {tipo_fn; formais} ->
           (* Infere o tipo de cada um dos argumentos *)
           let argst = List.map (infere_exp amb) args
           (* Obtem o tipo de cada parâmetro formal *)
           and tipos_formais = List.map snd formais in
           (* Verifica se o tipo de cada argumento confere com o tipo declarado *)
           (* do parâmetro formal correspondente.                               *)
           let _ = verifica_parametros args (List.map snd argst) tipos_formais
            in (T.ExpChamadaF (id, (List.map fst argst), tipo_fn), tipo_fn)
         | Amb.EntVar _ -> (* Se estiver associada a uma variável, falhe *)
           let msg = id ^ " eh uma variavel e nao uma funcao" in
           failwith (msg_erro nome msg)
       end
     with Not_found ->
       let msg = "Nao existe a funcao de nome " ^ id in
       failwith (msg_erro nome msg)

let rec verifica_cmd amb tiporet cmd =
  let open A in
  match cmd with
    CmdRetorno exp ->
    (match exp with
     (* Se a função não retornar nada, verifica se ela foi declarada como void *)
       None ->
       let _ = mesmo_tipo (Lexing.dummy_pos)
                   "O tipo retornado eh %s mas foi declarado como %s"
                   TipoVoid tiporet
       in CmdRetorno None
     | Some e ->
       (* Verifica se o tipo inferido para a expressão de retorno confere com o *)
       (* tipo declarado para a função.                                         *)
           let (e1,tinf) = infere_exp amb e in
           let _ = mesmo_tipo (posicao e)
                              "O tipo retornado eh %s mas foi declarado como %s"
                              tinf tiporet
           in CmdRetorno (Some e1)
      )
  | CmdIf (teste, entao, senao) ->
    let (teste1,tinf) = infere_exp amb teste in
    (* O tipo inferido para a expressão 'teste' do condicional deve ser booleano *)
    let _ = mesmo_tipo (posicao teste)
             "O teste do if deveria ser do tipo %s e nao %s"
             TypeBoolean tinf in
    (* Verifica a validade de cada comando do bloco 'então' *)
    let entao1 = List.map (verifica_cmd amb tiporet) entao in
    (* Verifica a validade de cada comando do bloco 'senão', se houver *)
    let senao1 =
        match senao with
          None -> None
        | Some bloco -> Some (List.map (verifica_cmd amb tiporet) bloco)
     in
     CmdIf (teste1, entao1, senao1)

  | CmdCase (variavel, casos, senao) ->
    let (variavel1, tinf1) = infere_exp amb variavel in
    let teste bloco = 
        match bloco with
          | Case (exp2, exp3) ->
              let (e2, tinf2) = infere_exp amb exp2 in
                let _ = mesmo_tipo (posicao variavel)
                    "O teste do case %s deve ser do mesmo tipo da variavel a ser analizada %s"
                tinf2 tinf1
                in let e3 = verifica_cmd amb tiporet exp3 
                in Case (e2, e3)
    in let casos1 = (List.map(teste) casos)
    in let senao1 = 
     match senao with
          None -> None
        | Some bloco -> Some (verifica_cmd amb tiporet bloco) 
    in
    CmdCase (variavel1, casos1, senao1)

  | CmdWhile (teste, cmd) ->
    let (teste1, tinf) = infere_exp amb teste in
    let _ = mesmo_tipo (posicao teste)
            "O teste do while deveria ser do tipo %s e nao %s"
           TypeBoolean tinf in
    let cmd1 = (List.map(verifica_cmd amb tiporet) cmd) in
    CmdWhile(teste1, cmd1)

    | CmdFor (variavel, primeiroelem, segundoelem, faca) ->
      let (variavel1,tinf) = infere_exp amb variavel in
      (* Variavel do for deve ser inteiro *)
      let (primeiroelem1,pelem) = infere_exp amb primeiroelem in
      let _ = mesmo_tipo (posicao variavel)
              "O inicio deve ser do mesmo tipo da variavel inicial %s, porem foi declarada como %s"
              TypeInteger pelem in
      let (segundoelem1,selem) = infere_exp amb segundoelem in
      let _ = mesmo_tipo (posicao variavel)
              "O final deve ser do mesmo tipo da variavel inicial %s, porem foi declarada como %s"
              TypeInteger selem in
      (* Variavel do for deve ser inteiro *)
      let _ = mesmo_tipo (posicao variavel)
              "A variavel do for deveria ser do tipo %s e nao %s"
              TypeInteger tinf in
      
      (* Verifica a validade de cada comando do bloco 'faca' *)
      let faca1 = List.map (verifica_cmd amb tiporet) faca in
      CmdFor (variavel1, primeiroelem1, segundoelem1, faca1)


  | CmdAtrib (elem, exp) ->
    (* Infere o tipo da expressão no lado direito da atribuição *)
    let (exp,  tdir) = infere_exp amb exp
    (* Faz o mesmo para o lado esquerdo *)
    and (elem1, tesq) = infere_exp amb elem in
    (* Os dois tipos devem ser iguais *)
    let _ = mesmo_tipo (posicao elem)
                       "Atribuicao com tipos diferentes: %s = %s" tesq tdir
    in CmdAtrib (elem1, exp)

  | CmdFunctionCall exp ->
     let (exp,tinf) = infere_exp amb exp in
     CmdFunctionCall exp

  | CmdRead exps ->
    (* Verifica o tipo de cada argumento da função 'entrada' *)
    let exps = List.map (infere_exp amb) exps in
    CmdRead (List.map fst exps)

  | CmdReadLn exps ->
    (* Verifica o tipo de cada argumento da função 'entradaln' *)
    let exps = List.map (infere_exp amb) exps in
    CmdReadLn (List.map fst exps)

  | CmdWrite exps ->
    (* Verifica o tipo de cada argumento da função 'saida' *)
    let exps = List.map (infere_exp amb) exps in
    CmdWrite (List.map fst exps)

  | CmdWriteLn exps ->
    (* Verifica o tipo de cada argumento da função 'saidaln' *)
    let exps = List.map (infere_exp amb) exps in
    CmdWriteLn (List.map fst exps)

and verifica_fun amb ast =
  let open A in
  match ast with
    A.Function {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
    (* Estende o ambiente global, adicionando um ambiente local *)
    let ambfn = Amb.novo_escopo amb in
    (* Insere os parâmetros no novo ambiente *)
    let insere_parametro (v,t) = Amb.insere_param ambfn (fst v) t in
    let _ = List.iter insere_parametro fn_formais in
    (* Insere as variáveis locais no novo ambiente *)
    let insere_local = function
        (DecVar (v,t)) -> Amb.insere_local ambfn (fst v)  t in
    let _ = List.iter insere_local fn_locais in
    (* Verifica cada comando presente no corpo da função usando o novo ambiente *)
    let corpo_tipado = List.map (verifica_cmd ambfn fn_tiporet) fn_corpo in
      A.Function {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo = corpo_tipado}


let rec verifica_dup xs =
  match xs with
    [] -> []
  | (nome,t)::xs ->
    let id = fst nome in
    if (List.for_all (fun (n,t) -> (fst n) <> id) xs)
    then (id, t) :: verifica_dup xs
    else let msg = "Parametro duplicado " ^ id in
      failwith (msg_erro nome msg)

let insere_declaracao_var amb dec =
  let open A in
    match dec with
        DecVar (nome, tipo) ->  Amb.insere_local amb (fst nome) tipo

let insere_declaracao_fun amb dec =
  let open A in
    match dec with
      Function {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
        (* Verifica se não há parâmetros duplicados *)
        let formais = verifica_dup fn_formais in
        let nome = fst fn_nome in
        Amb.insere_fun amb nome formais fn_tiporet


(* Lista de cabeçalhos das funções pré definidas *)
let fn_predefs = let open A in [
   ("entrada", [("x", TypeInteger); ("y", TypeInteger)], TipoVoid);
   ("saida",   [("x", TypeInteger); ("y", TypeInteger)], TipoVoid);
   ("entradaln", [("x", TypeInteger); ("y", TypeInteger)], TipoVoid);
   ("saidaln",   [("x", TypeInteger); ("y", TypeInteger)], TipoVoid)
]

(* insere as funções pré definidas no ambiente global *)
let declara_predefinidas amb =
  List.iter (fun (n,ps,tr) -> Amb.insere_fun amb n ps tr) fn_predefs

let semantico ast =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in
  let _ = declara_predefinidas amb_global in
  let (A.Program (id, decs_globais, decs_funs, corpo)) = ast in
  let _ = List.iter (insere_declaracao_var amb_global) decs_globais in
  let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
  (* Verificação de tipos nas funções *)
  let decs_funs = List.map (verifica_fun amb_global) decs_funs in
  (* Verificação de tipos na função principal *)
  let corpo = List.map (verifica_cmd amb_global A.TipoVoid) corpo in
     (A.Program (id, decs_globais, decs_funs, corpo),  amb_global)