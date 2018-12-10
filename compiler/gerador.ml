open Printf
open Ast
open Tast
open Codigo

let num_bytes t =
    match t with
      TypeInt -> 4
    | TipoString -> 4
    | TipoBool -> 4
    | TipoVoid -> 4
    | TipoArranjo (t,i,f) -> 4
    | TipoRegistro cs -> 4


let emite_cabecalho oc arq = 
  fprintf oc "	.file	\"%s\"\n" arq

let rec emite_global oc cod =
  match cod with
  | Global (n,t) :: cod ->
    let _ = fprintf oc "	.comm	%s,%d,4\n" n (num_bytes t) in
    emite_global oc cod
  | _ -> 
    let _ = fprintf oc "	.text\n" in 
    cod

let emite_prologo oc nome =
  fprintf oc "
	.globl	%s
	.type	%s, @function
%s:
	pushq	%%rbp
	movq	%%rsp, %%rbp
"
  nome nome nome

let emite_epilogo oc nome =
  fprintf oc "	.size	%s, .-%s\n" nome nome

let rec move_parametros oc tbl cod regs =  
  match cod with
  | Recebe (n,tipo) :: cod -> 
    let deslocamento = Hashtbl.find tbl (Nome n) in
    let _ = fprintf oc "	movl	%%%s, %d(%%rbp)      # %s\n" 
                   (List.hd regs) deslocamento n
    in move_parametros oc tbl cod (List.tl regs)
  | _ -> cod


let rec emite_corpo oc tbl cod = 
  match cod with
  | Local _ :: cod-> emite_corpo oc tbl cod
  | Copia (Nome n, ConstInt i) :: cod ->
    begin 
    try
        let deslocamento = Hashtbl.find tbl (Nome n) in
        let _ =  fprintf oc "	movl	$%d, %d(%%rbp)        # %s <- %d\n" 
                            i deslocamento n i
        in emite_corpo oc tbl cod
    with 
        Not_found -> (* Assume que é uma variável global *)
        let _ =  fprintf oc "	movl	$%d, %s(%%rip)        # %s <- %d\n" 
                            i n n i
        in emite_corpo oc tbl cod  
    end      
  | Return opcao :: cod ->
    let _ = 
      (match opcao with
       | None -> ()
       | Some endr ->
          (match endr with
          | Nome n -> 
            begin 
                try
                let deslocamento = Hashtbl.find tbl endr in
                fprintf oc "	movl	%d(%%rbp), %%eax       # EAX <- %s\n" 
                           deslocamento n
                with 
                  Not_found -> (* Assume que é uma variável global *)
                    fprintf oc "	movl	%s(%%rip), %%eax       # EAX <- %s\n" 
                                n n
            end
          | _ -> failwith "emite_corpo: endereco nao implementado"
          )
       )
    in emite_corpo oc tbl cod
  | EndFun :: cod -> 
    let _ =  fprintf oc "	leave\n	ret\n" 
    in cod
  | _ :: cod -> emite_corpo oc tbl cod
  | [] -> []
  
 
  
let emite_quadro_de_pilha oc cod nargs nlocais =
  let cria_quadro_de_pilha tbl params locais =
      let rec aloca_na_pilha inicio fim xs =
        match xs with
        | (x,nb) :: restante -> 
           let endereco = inicio - nb in
           if endereco >= fim 
           then let _ = Hashtbl.add tbl (Nome x) endereco in
                aloca_na_pilha endereco fim restante
           else aloca_na_pilha fim (fim - 16) xs
        | [] -> fim
      in 
         let fim_locais = aloca_na_pilha 0 (-16) locais in
         let fim_params = aloca_na_pilha fim_locais (fim_locais - 16) params in
         fprintf oc "	subq	$%d, %%rsp\n" (- fim_params)
  in  
  let rec separa_parametros n params cod =
     if n <= 0 then (List.rev params, cod)
     else match cod with
          | Recebe (nome,tipo) :: cod -> 
            let nb = num_bytes tipo in 
            separa_parametros (n-1) ((nome,nb) :: params) cod
          | _ -> (List.rev params, cod)
  and separa_locais n locais cod =
     if n <= 0 then (locais, cod)
     else match cod with
          | Local (nome,tipo) :: cod -> 
            let nb = num_bytes tipo in 
            separa_locais (n-1) ((nome,nb) :: locais) cod
          | _ -> (locais, cod)
  in 
    let (params, cod) = separa_parametros nargs [] cod in
    let (locais, cod) = separa_locais nlocais [] cod in
    let tbl_de_alocacao = Hashtbl.create 5 in 
    let _ = cria_quadro_de_pilha tbl_de_alocacao params locais in
    tbl_de_alocacao

let emite_funcao oc cod nome nargs nlocais =
     let _ = emite_prologo oc nome in
     let tbl = emite_quadro_de_pilha oc cod nargs nlocais in
     let registradores = ["edi"; "esi"; "edx"; "ecx"; "r8d"; "r9d"] in
     let cod = move_parametros oc tbl cod registradores in
     let cod = emite_corpo oc tbl cod in
     let _ = emite_epilogo oc nome in
     cod

let emite_rodape oc =
  fprintf oc "
	.ident	\"GCC: (Ubuntu 6.3.0-12ubuntu2) 6.3.0 20170406\"
	.section	.note.GNU-stack,\"\",@progbits
"  

let rec emite_codigo oc cod = 
  match cod with
  | BeginFun (nome, nargs, nlocais) :: cod ->
    let cod = emite_funcao oc cod nome nargs nlocais in
    emite_codigo oc cod
  | _ -> ()

let gerador oc cod arq =
  let _ = emite_cabecalho oc arq in
  let cod = emite_global oc cod in
  let _ = emite_codigo oc cod in
  emite_rodape oc 
  
let compila arq =  
  let oc = open_out (Filename.chop_suffix arq ".tip" ^ ".s") in
  let cod = traduz arq in
    gerador stdout cod arq;
    close_out oc
