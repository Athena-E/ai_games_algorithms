type tree = Lf of int | Br of int * tree list;;

let depth = 4;; 
let cutoff t = false;; 

let max x y = if x > y then x else y;;
let min x y = if x < y then x else y;; 

let my_tree = 
  Br(0,[
      Br(0, [
          Br(0,[
              Br(0,[
                  Lf(1);
                  Lf(-15) 
                ]);
              Br(0,[
                  Lf(2);
                  Lf(19)
                ])
            ]);
          Br(0,[
              Br(0,[
                  Lf(18);
                  Lf(23)
                ]);
              Lf(4);
              Lf(3)
            ])
        ]);
      Br(0, [
          Br(0,[
              Br(0,[
                  Lf(2);
                  Lf(1) 
                ]);
              Br(0,[
                  Lf(7);
                  Lf(8)
                ])
            ]);
          Br(0,[
              Br(0,[
                  Lf(9);
                  Lf(10)
                ]);
              Lf(-2);
              Lf(5)
            ])
        ]); 
      Br(0, [
          Br(0,[
              Br(0,[
                  Lf(-1);
                  Lf(-30) 
                ]);
              Br(0,[
                  Lf(4);
                  Lf(7)
                ])
            ]);
          Br(0,[
              Br(0,[
                  Lf(20);
                  Lf(-1)
                ]);
              Lf(-1);
              Lf(-5)
            ])
        ])
    ])
    

let rec player alpha beta t =
  match t with
  | Lf(x) -> x
  | Br(x,children) ->
      if cutoff t then x
      else 
        let value = min_int in
        let rec aux alpha beta value children = 
          match children with
          | [] -> value
          | th::tt -> 
              let new_value = max value (opponent alpha beta th) in
              if new_value >= beta then new_value 
              else if new_value > alpha then aux new_value beta new_value tt
              else aux alpha beta new_value tt 
        in aux alpha beta value children 
and opponent alpha beta t =
  match t with
  | Lf(x) -> x
  | Br(x,children) ->
      if cutoff t then x
      else
        let value = max_int in
        let rec aux alpha beta value children = 
          match children with
          | [] -> value
          | th::tt -> 
              let new_value = min value (player alpha beta th) in
              if new_value <= alpha then new_value
              else if new_value < beta then aux alpha beta new_value tt
              else aux alpha beta new_value tt
        in aux alpha beta value children;;
              
              
player (min_int) (max_int) my_tree;;
opponent (min_int) (max_int) my_tree;;               
              
              
              
              
              
  
