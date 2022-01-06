#!/usr/bin/env -S ocaml str.cma

(* Read .atoc file line-by-line to produce a table of contents
   and an author index. The author index is put into alphabetical
   order, but manual revision is necessary to properly treat different
   linguistic conventions.
   (It does apply rules for the western European particles "van", "van der",
   "von", "de", and "du".

   usage: ./artitoc.ml main.atoc

   2022 T.Bourke
 *)

(* options *)

(* and_text: separator for the last author in a group of 3 or more. *)
(* and2_text: separator for the second of two authors. *)
let and_text, and2_text = ", and ", " and "
(* let and_text, and2_text = " et ", " et " *)


(* Main data structures *)

let authors = Hashtbl.create 100

let add_author_page name page =
  match Hashtbl.find_opt authors name with
  | Some pages -> Hashtbl.replace authors name (page::pages)
  | None -> Hashtbl.add authors name [page]

type article = {
    title : string;
    page : int;
    mutable authors : string list;
  }

let articles = ref []

let new_article title page =
  articles := { title; page; authors = [] } :: !articles

let add_article_author name =
  match !articles with
  | [] -> () (* silently ignore *)
  | article :: _ ->
      article.authors <- name :: article.authors

(* Parse file produced by LaTeX macros *)

let title_line = Str.regexp "^title:\\([0-9][0-9]*\\):\\(.*\\)$"
let author_line = Str.regexp "^author:\\([0-9][0-9]*\\):\\([^:]*\\):\\(.*\\)$"

let read_lines in_ch process =
  let rec read_line n =
    try
      process n (input_line in_ch);
      read_line (n + 1);
    with End_of_file -> ()
  in read_line 1

let process_title page title =
  new_article title page

let process_author page first last =
  add_author_page (last ^ ", " ^ first) page;
  add_article_author (first ^ " " ^ last)

let process_line n s =
  if Str.string_match title_line s 0
  then process_title (int_of_string (Str.matched_group 1 s))
                     (Str.matched_group 2 s)
  else if Str.string_match author_line s 0
  then process_author (int_of_string (Str.matched_group 1 s))
                      (Str.matched_group 2 s)
                      (Str.matched_group 3 s)
  else Printf.eprintf "line %d: could not parse\n" n

(* Produce the table of contents *)

let rec output_long_author_list och = function
  | [] -> assert false
  | [x] ->
      (output_string och and_text;
       output_string och x)
  | x::xs ->
      (Printf.fprintf och ", %s" x;
       output_long_author_list och xs)

let output_author_list och = function
  | [] -> ()
  | [x] ->
      output_string och x
  | [x1; x2] ->
      (output_string och x1;
       output_string och and2_text;
       output_string och x2)
  | x::xs ->
      (output_string och x;
       output_long_author_list och xs)

let output_toc och =
  let print_article { title; page; authors } =
    Printf.fprintf och "  \\tocTitle{%s}{%d}\n" title page;
    Printf.fprintf och "  \\tocAuthors{%a}\n"
                       output_author_list (List.rev authors)
  in
  List.iter print_article (List.rev !articles)

(* Produce the author index *)

(* let output_page_number och p = Printf.fprintf och "%d" p *)

let output_page_number och p =
  Printf.fprintf och "\\hyperlink{page.%d}{%d}" p p

let rec output_other_pages och = function
  | [] -> ()
  | p::ps ->
      Printf.fprintf och ", %a" output_page_number p;
      output_other_pages och ps

let output_page_list och = function
  | [] -> ()
  | p::ps ->
      output_page_number och p;
      output_other_pages och ps

let output_author_entry och (_, author, pages) =
  Printf.fprintf och "%s \\dotfill & %a \\\\\n"
                     author
                     output_page_list (List.rev pages)

(* NB: the sorting of name is very naïve. It could be improved by:
   1. Properly comparing unicode characters.
   2. Taking more linguistic conventions into account.
      E.g., for multiple last names, sometimes the second should be
      used
      https://www.chicagomanualofstyle.org/qanda/data/faq/topics/Alphabetizing/faq0015.html
      othertimes the first should be used (many Spanish last names).
      Ultimately, this probably has to be manually verified.
 *)

let particles =
  List.map (fun (s, r) -> Str.regexp s, String.length s, r)
  [
    ("de ", "");
    ("van der ", "");
    ("van ", "");
    ("von ", "");
    ("du ", "du");
    ("Du ", "Du");
  ]

let for_sorting author_name =
  let rec check_particles = function
    | [] -> author_name
    | (p, plen, r)::_ when Str.string_match p author_name 0 ->
        r ^ String.(sub author_name plen (length author_name - plen))
    | _::ps -> check_particles ps
  in
  String.capitalize_ascii (check_particles particles)

let output_author_index och =
  Hashtbl.fold (fun k v a -> (for_sorting k, k, v) :: a) authors []
  |> List.sort (fun (a1, _, _) (a2, _, _) -> String.(compare a1 a2))
  |> List.iter (output_author_entry och)

(* Command-line arguments and main *)

let atoc_file = ref ""
let toc_file = ref ""
let authors_file = ref ""

let set_atoc_file s =
  atoc_file := s

let args = Arg.[
  ("--toc", Set_string toc_file,
   "<path> Output the table-of-contents to a file.");
  ("--authors", Set_string authors_file,
   "<path> Output the author index to a file.");
]

let main () =
  Arg.parse args set_atoc_file "generate table-of-contents and author index";
  let ich = if !atoc_file = "" then stdin else open_in !atoc_file in
  read_lines ich process_line;
  if !toc_file <> ""
    then let och = open_out !toc_file in
         (output_toc och; close_out och)
    else output_toc stdout;
  if !authors_file <> ""
    then let och = open_out !authors_file in
         (output_author_index och; close_out och)
    else output_author_index stdout

let _ = main ()

