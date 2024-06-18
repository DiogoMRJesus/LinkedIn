open Z

(*Criação do Array que irá guardar os valores já calculados para otimizar*)

let array = Array.make 10001 Z.zero

(*Função recursiva que irá cálcular o número de Motzkin*)

let rec motzkin n = 

  if n < Z.of_int 2 then Z.one

  else if array.(Z.to_int n) > Z.zero then array.(Z.to_int n) 

  else

    let dn1 = Z.mul (Z.of_int 3) (Z.sub n (Z.of_int 1)) in (*Função para 3*(n-1)*)

    let dn2 = Z.add (Z.mul (Z.of_int 2) n) (Z.of_int 1) in (*Função para (2n+1)*)

    let dn3 = Z.add n (Z.of_int 2) in (*Função para (n+2)*)

    let mn1 = Z.sub n (Z.of_int 2) in (*Função para (n-2)*)

    let mn2 = Z.sub n (Z.of_int 1) in (*Função para (n-1)*)

    let resultado = Z.div ( Z.add ( Z.mul (dn1) (motzkin (mn1))) (Z.mul (dn2) (motzkin (mn2)))) (dn3) in (*Função para o resultado final*) 

    array.(Z.to_int n) <- resultado;(*Atribuição do valor calculado ao seu respectivo slot no Array*)

    resultado

(*Função Principal onde irá ocorrer a leitura e escrita do input e output*)

let () =

  let input = Z.of_int (read_int ()) in 

  if input <= Z.of_int 10000 && input >= Z.zero then 

  let output = motzkin input in 

  print_string (Z.to_string output);

  print_endline ""



    (*

    Diogo Jesus nº 49676

    Gonçalo Caixeiro nº 49758

    Fontes: https://v2.ocaml.org/manual/index.html

            https://stackoverflow.com/questions/71889378/how-should-i-implement-this-algorithm-with-zarith-because-it-only-works-till-26

            Material fornecido pelo professor

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////   

     Este programa é dividido em três partes:

    - o cálculo do número de Motzkin;
      O valor é calculado pela expressão recursiva «motzkin» que se divide em dois blocos através de uma instrução condicional, caso o número
      pedido seja menor que 2, o valor do output será dado automaticamente 1 como está definido na expressão,
      caso contrário (seja superior ou igual a 2), iremos passar para o segundo bloco, onde o número de motzkin será calculado.
      Neste bloco, foi colocada uma expressão que traduz o cálculo do número de motzkin.
      Esta expressão foi segmentada em várias subfunções a fim de facilitar a leitura do código na passagem para Zarith
      (biblioteca utilizada para «lidar» com números extensos) prevenindo assim erros.
      Por sua vez estas subfunções, irão ser chamdas pela função de Motzkin que irá, recursivamente, chamar as funções de motzkin
      dos valores inferiores até alcançar um valor conhecido, neste caso 1 e prosseguir com os cálculos.
     
    - a memorização do valor;
      Visto que estamos a trabalhar com números muito extensos, foi necessário a memorização dos valores calculados
      para diminuir o tempo de execução do programa, visto que a maior parte do tempo de execução do program deve-se
      ao cálculo do número de motzkin.
      Daí foi necessário adicionar ao programa um Array, denominado: «array», que serviu para guardar os valores calculados 
      anteriormente pela função, dado que para o cálculo de um número de Motzkin muito extenso, vai ser necessário calcular 
      várias vezes o mesmo valor, (Exemplo: Moktzkin 4 - Necessita de calcular 2x o Motzkin de 2) e quanto maior for o valor
      mais repetições irão ocorrer, logo sempre que for necessário calcular um valor, primeiro será verificado naquela posição
      específica do array se esse número é maior que zero (ou seja, se nessa posição está guardado algum valor), caso esteja guardado, o 
      programa utiliza o valor anteriormente calculado, caso contrário prossgue para a função onde se vê-se obrigado a calculá-lo.
      Por fim, este valor é guardado na posição «n» correspondente do array para ser utilizado caso seja necessário.


    - leitura (de números de 0 até 10000) e apresentação de dados .
      Neste programa é recebido um input, do tipo inteiro que é posteriormente convertido para a biblioteca «Zarith».
      Por sua vez é testado se o valor do input está dentro do intervalo desejado, se for falso, o programa acaba, caso 
      contrário o programa prosseguirá, onde irá introduzir o valor «input» na punção recursiva «motzkin» e o seu valor 
      será guardado na variável «output».
      Por fim, este valor será imprimido, através do «print_string» e será feita uma quebra de linha , para colocar o «output» 
      do programa na linha seguinte.




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Demonstração do cálculo do número de Motzkin

    «Input»

    5

    «Cálculo do número de Motzkin e a sua apresentação»

    21     

    «Final do Programa»



    Demonstração do cálculo do número de Motzkin caso o número introduzido esteja fora do intervalo imposto 

    «Input»

    -3

    «Não há output pois o número é menor que 0»

    «Final do Programa»

    *)

