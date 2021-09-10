let () = print_endline "\n***| Lista de Idosos com Respetivo Grau de Risco |***\n" (* Initial Message *)

let ver_linha_entrada canal_entrada =   (* check input channel lines*)
  try Some (input_line canal_entrada) (* reads from the input channel all lines to the end returns in list form *)
  with End_of_file -> None (* When there are no more characters to read, close the input channel.*)
 
let ler_linhas canal_entrada = (*get lines from input channel*)
  let rec auxiliar acc = (* auxiliary function with accumulator, uses accumulator to store the resulting list on each recursive call *)
    match ver_linha_entrada canal_entrada with 
    | Some linha -> auxiliar (linha::acc)
    | None -> (List.rev acc) (* call the list reversal *)
  in
  auxiliar []
 
let obter_linhas_ficheiro nome_ficheiro = (*get threads from the file*)
  let canal_entrada = open_in nome_ficheiro in (*open file for input channel*)
  let linhas = ler_linhas canal_entrada in (*read the input channel lines to the list*)
  close_in canal_entrada;
  (linhas)

let linhas = obter_linhas_ficheiro "seniors.txt" (*calls the read and saves the read lines of the file in the list*)

let parcelar_dados linha = (*do a pass the line ie divide the string into the elements after the ; in various elements*)
  String.split_on_char ';' linha

let obter_palavra lista_palavras k = (*access a specific element of the list*)
  List.nth lista_palavras k

let controlar linha_recebida igualdade parametro= (*function that compares the equality of elements*)
  let palavras = parcelar_dados linha_recebida in (*splits the string into elements after the ;*)
  let palavra = obter_palavra palavras parametro in (*access the element of the previous division you want to compare*)
  String.compare palavra igualdade (*make the comparison*)

let obter_nome linha_recebida parametro= (*function that compares the equality of elements to print the name*)
  let palavras = parcelar_dados linha_recebida in (*splits the string into elements after the ;*)
  let palavra = obter_palavra palavras parametro in (*access the element of the previous division you want to compare*)
  Printf.printf "%s\n" palavra (*Imprime o parametro nome*)

let rec lerlista lista opcao= ( (*Recursively reads the list and uses match to step through the elements presents the risk through the conditions at each node in the list.*)
  match lista with
  | [] -> []
  | h::t ->
    begin
    if (opcao==1 || opcao==4) && (controlar h "2" 1)>0 && (controlar h "3" 2)>0 && (controlar h "s" 4)==0 && (controlar h "n" 6)==0 && (controlar h "n" 8)==0 && (controlar h "baixo" 10)!=0 then
      begin
        Printf.printf "%s - " "Risco: Alto          ";
        (obter_nome h 0);
      end
    else if (opcao==2 || opcao==4) && (controlar h "3" 1)<0 && (controlar h "4" 2)<0 && (controlar h "n" 7)==0 && (controlar h "n" 9)==0 && (controlar h "alto" 10)!=0 then
      begin
        Printf.printf "%s - " "Risco: Medio         ";
        (obter_nome h 0)
      end
    else if (opcao==3 || opcao==4) && (controlar h "0" 1)==0 && (controlar h "3" 2)<0 && (controlar h "n" 3)==0 && (controlar h "n" 5)==0 && (controlar h "baixo" 10)==0 then
      begin
        Printf.printf "%s - " "Risco: Baixo         ";
        (obter_nome h 0)
      end
    else if opcao==4 then
      begin
        Printf.printf "%s - " "Risco: Nao Enquadrado";
        (obter_nome h 0)
      end
    else if opcao==0 then
      begin
        Printf.printf "\n%s\n" "Adeus";
        exit 0;
      end
    end
    ::(lerlista t opcao)
  );;

print_string "Escolha a forma de apresentação dos dados:\n1 - Apenas Alto Risco\n2 - Apenas Médio Risco\n3 - Apenas Baixo Risco\n4 - Todos\n0 - Sair\n\nInsira a sua opcao: ";
let opcao = read_int () in (*Collect user option*)
  if opcao!=0 then
    begin
    (lerlista linhas opcao)
    end
  else
    begin
      print_string "Adeus";
      exit 0;
    end