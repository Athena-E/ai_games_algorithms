module Node = struct
  type t = int
  let compare = Int.compare
  let to_string = string_of_int
end

module NodeMap = Map.Make(Node)
module NodeSet = Set.Make(Node)

module Colour = struct
  type t = R | C | B
  let compare = Stdlib.compare 
  let to_string = function R -> "R" | C -> "C" | B -> "B"
end

module ColourSet = Set.Make(Colour)

type graph = NodeSet.t NodeMap.t

type domains_map = ColourSet.t NodeMap.t

    
let remove_inconsistencies_for_arc (node_i : Node.t) (node_j : Node.t) (domains_map_ref : domains_map ref) : bool =
  let current_domains = !domains_map_ref in
  
  let domain_i_orig = NodeMap.find node_i current_domains in
  let domain_j = NodeMap.find node_j current_domains in
  
  let new_domain_i = 
    ColourSet.filter (fun color_i -> 
        ColourSet.exists (fun color_j -> color_i <> color_j) domain_j) domain_i_orig
  in 
  if not (ColourSet.equal new_domain_i domain_i_orig) then (
    domains_map_ref := NodeMap.add node_i new_domain_i current_domains;
    true 
  ) else false
    
    
let ac3 (all_problem_nodes : Node.t list) (graph_adj : graph) (initial_domains : domains_map) 
  : (bool * domains_map) =
  
  let domains_ref = ref initial_domains in
  let queue = Queue.create () in

  List.iter (fun node_i ->
      match NodeMap.find_opt node_i graph_adj with
      | Some neighbors_of_i ->
          NodeSet.iter (fun node_j ->
              Queue.add (node_i, node_j) queue (* arc i -> j *)
            ) neighbors_of_i
      | None -> () 
    ) all_problem_nodes; 

  let inconsistency_found = ref false in

  while not (Queue.is_empty queue) && not !inconsistency_found do
    let (node_i, node_j) = Queue.pop queue in

    if remove_inconsistencies_for_arc node_i node_j domains_ref then ( 
      let updated_node_i_domain = NodeMap.find node_i !domains_ref in
      if ColourSet.is_empty updated_node_i_domain then ( 
        inconsistency_found := true;
      ) else ( 
        match NodeMap.find_opt node_i graph_adj with
        | Some neighbors_of_i ->
            NodeSet.iter (fun neighbor_k -> 
                if Node.compare neighbor_k node_j <> 0 then ( 
                  Queue.add (neighbor_k, node_i) queue 
                )
              ) neighbors_of_i
        | None -> () 
      )
    )
  done; 
  (not !inconsistency_found, !domains_ref)

  
(*printing functions*)
let print_color_set formatter s =
  Format.fprintf formatter "{ ";
  ColourSet.iter (fun c -> Format.fprintf formatter "%s " (Colour.to_string c)) s;
  Format.fprintf formatter "}"

let print_domains formatter domains_map =
  NodeMap.iter (fun node colors ->
      Format.fprintf formatter "D(%s) = %a\n" (Node.to_string node) print_color_set colors;
    ) domains_map 
    

(*problem description*)
let problem_nodes_list : Node.t list = [1; 2; 3; 4; 5; 6; 7; 8]

let problem_adj_graph : graph =
  NodeMap.empty
  |> NodeMap.add 1 (NodeSet.of_list [2; 3; 4])
  |> NodeMap.add 2 (NodeSet.of_list [1; 4; 6])
  |> NodeMap.add 3 (NodeSet.of_list [1; 4; 7])
  |> NodeMap.add 4 (NodeSet.of_list [1; 2; 3; 5])
  |> NodeMap.add 5 (NodeSet.of_list [4; 6; 7])
  |> NodeMap.add 6 (NodeSet.of_list [2; 5; 7; 8])
  |> NodeMap.add 7 (NodeSet.of_list [3; 5; 6; 8])
  |> NodeMap.add 8 (NodeSet.of_list [6; 7])

let all_colors_list = [Colour.R; Colour.C; Colour.B]
let all_colors_set = ColourSet.of_list all_colors_list

(* state of domains after 5=R *)
let initial_domains : domains_map =
  let base_map = 
    NodeMap.empty
    |> NodeMap.add 1 (ColourSet.singleton Colour.R)
    |> NodeMap.add 2 (ColourSet.singleton Colour.B)
    |> NodeMap.add 3 (ColourSet.singleton Colour.B)
    |> NodeMap.add 4 (ColourSet.singleton Colour.C)
    |> NodeMap.add 5 (ColourSet.singleton Colour.R)
    |> NodeMap.add 6 (ColourSet.singleton Colour.C)
    |> NodeMap.add 7 (ColourSet.singleton Colour.C)
    |> NodeMap.add 8 (ColourSet.of_list [Colour.R;Colour.B;Colour.C])
  in 
  List.fold_left (fun current_map node_id ->
      if NodeMap.mem node_id current_map then current_map
      else NodeMap.add node_id all_colors_set current_map
    ) base_map problem_nodes_list

    
(* full initial domains *)
let full_initial_domains : domains_map =
  List.fold_left (fun acc_map node_id ->
      NodeMap.add node_id all_colors_set acc_map
    ) NodeMap.empty problem_nodes_list

let assignments = [(1,Colour.R);(4,Colour.C);(5,Colour.R);(8,Colour.C);(6,Colour.B)]    

(* main execution *)
let () =
  let doms_ref = ref full_initial_domains in
  let rec main_loop assignments = 
    match assignments with
    | [] -> ()
    | (n,c)::t -> 
        Format.printf "----------------------------------------\n";
        doms_ref := NodeMap.add n (ColourSet.singleton c) (!doms_ref);
        Format.printf "Domains after %s=%s assignment:\n%a" (string_of_int n) (Colour.to_string c) print_domains (!doms_ref);
        Format.printf "----------------------------------------";
  
        let is_consistent, final_domains = 
          ac3 problem_nodes_list problem_adj_graph (!doms_ref)
        in 
        if is_consistent then
          Format.printf "\nCSP is arc-consistent.\n"
        else
          Format.printf "\nInconsistency detected.\n";
        Format.printf "Final domains state:\n%a" print_domains final_domains;
        Format.printf "----------------------------------------\n\n";
        if is_consistent then main_loop t else () 
  in
  main_loop assignments
    


  
  
  
