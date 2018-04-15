type  g_context = {
     mutable bcol : Graphics.color;
     mutable fcol : Graphics.color;
     mutable font :  string;
     mutable font_size : int;
     mutable lw : int;
     mutable x : int;
     mutable y : int }

let default_font = "fixed"
let default_font_size = 12

let make_default_context () =
  { bcol = Graphics.white; fcol = Graphics.black;
    font = default_font;
    font_size = default_font_size;
    lw = 1;
    x = 0; y = 0;}

let get_gc_bcol gc  = gc.bcol
let get_gc_fcol gc  = gc.fcol
let get_gc_font  gc  = gc.font
let get_gc_font_size  gc  = gc.font_size
let get_gc_lw gc  = gc.lw
let get_gc_cur gc = (gc.x,gc.y)

let set_gc_bcol gc c = gc.bcol <- c
let set_gc_fcol gc c = gc.fcol <- c
let set_gc_font  gc f = gc.font <- f
let set_gc_font_size  gc s = gc.font_size <- s
let set_gc_lw gc i = gc.lw <- i
let set_gc_cur gc (a,b) = gc.x<- a; gc.y<-b

let use_gc gc =
     Graphics.set_color (get_gc_fcol gc);
     Graphics.set_font (get_gc_font gc);
     Graphics.set_text_size (get_gc_font_size gc);
     Graphics.set_line_width (get_gc_lw gc);
     let (a,b) = get_gc_cur gc in Graphics.moveto a b

type rich_event =
  MouseDown | MouseUp | MouseDrag | MouseMove
  | MouseEnter | MouseExit | Exposure
  | GotFocus | LostFocus | KeyPress | KeyRelease

type opt_val = Copt of Graphics.color | Sopt of string
             | Iopt of int | Bopt of bool

type lopt = (string * opt_val) list

exception OptErr

let theInt lo name default =
   try
      match List.assoc name lo with
        Iopt i -> i
      | _  -> raise OptErr
   with Not_found -> default

let theBool lo name default =
  try
     match List.assoc name lo with
       Bopt b -> b
     | _  -> raise OptErr
  with Not_found -> default

let theString lo name default =
  try
     match List.assoc name lo with
       Sopt b -> b
     | _  -> raise OptErr
  with Not_found -> default

let theColor lo name default =
  try
     match List.assoc name lo with
       Copt b -> b
     | _  -> raise OptErr
  with Not_found -> default

let set_gc gc lopt =
  set_gc_bcol gc (theColor lopt "Background" (get_gc_bcol gc));
  set_gc_fcol gc (theColor lopt "Foreground" (get_gc_fcol gc));
  set_gc_font gc (theString lopt "Font" (get_gc_font gc));
  set_gc_font_size gc (theInt lopt "FontSize" (get_gc_font_size gc));
  set_gc_lw gc (theInt lopt "LineWidth" (get_gc_lw gc))

(* let dc = make_default_context () in
   set_gc dc [ "Foreground", Copt Graphics.blue;
               "Background", Copt Graphics.yellow] *)

type  component =
  { mutable info : string;
    mutable x : int;  mutable y : int;
    mutable w :int ; mutable h : int;
    mutable gc : g_context;
    mutable container : bool;
    mutable parent :   component list;
    mutable children :  component list;
    mutable layout_options : lopt;
    mutable layout : component ->  lopt -> unit;
    mutable display : unit  -> unit;
    mutable mem : int * int -> bool;
    mutable listener : rich_status  -> bool }
and rich_status =
  { re : rich_event;
    stat : Graphics.status;
    mutable key_focus : component;
    mutable gen_focus : component;
    mutable last : component}

let get_gc c = c.gc
let is_container c = c.container
let in_rect  c  (xp,yp)   =
  (xp >= c.x) && (xp < c.x + c.w) && (yp >= c.y) && (yp < c.y + c.h)

let display_rect c ()   =
   let gc = get_gc c in
     Graphics.set_color (get_gc_bcol gc);
     Graphics.fill_rect c.x c.y c.w c.h

let direct_layout c c1 lopt =
  let px = theInt lopt "PosX" 0
  and py = theInt lopt "PosY" 0 in
  c1.x <- c.x + px; c1.y <- c.y + py

let create_component iw ih  =
   let dc =
      {info="Anonymous";
       x=0; y=0; w=iw; h=ih;
       gc = make_default_context() ;
       container = false;
       parent = []; children = [];
       layout_options = [];
       layout = (fun a b -> ());
       display = (fun () -> ());
       mem = (fun s -> false);
       listener = (fun s  -> false);}
   in
     dc.layout <- direct_layout dc;
     dc.mem <- in_rect dc;
     dc.display <- display_rect dc;
     dc

let empty_component = create_component 0 0

let rec change_coord  c (dx,dy)   =
   c.x <- c.x + dx; c.y <- c.y + dy;
   List.iter (fun s -> change_coord s (dx,dy) ) c.children

let add_component  c c1 lopt   =
  if c1.parent <> [] then failwith "add_component: already a parent"
  else
    if not (is_container c ) then
      failwith "add_component: not a container"
    else
      if (c1.x + c1.w > c.w) || (c1.y + c1.h > c.h)
      then failwith "add_component: bad position"
      else
        c.layout  c1 lopt;
        c1.layout_options <- lopt;
        List.iter (fun s -> change_coord s (c1.x,c1.y)) c1.children;
        c.children <- c1::c.children;
        c1.parent <-  [c]

let remove_component c c1 =
     c.children <- List.filter ((!=) c1)  c.children;
     c1.parent <- List.filter ((!=) c) c1.parent;
     List.iter (fun s -> change_coord s (- c1.x, - c1.y)) c1.children;
     c1.x <- 0; c1.y <- 0

let set_layout f c =
  if c.children = [] then c.layout <- f
  else
    let ls = c.children in
      List.iter (remove_component c) ls;
      c.layout <- f;
      List.iter (fun s -> add_component c s s.layout_options) ls

let rec display  c   =
   c.display ();
   List.iter (fun cx -> display cx ) c.children


let get_event e = e.re
let get_mouse_x e = e.stat.Graphics.mouse_x
let get_mouse_y e = e.stat.Graphics.mouse_y
let get_key e = e.stat.Graphics.key

let has_key_focus e c = e.key_focus == c
let take_key_focus e c = e.key_focus <- c
let lose_key_focus e c = e.key_focus <- empty_component
let has_gen_focus e c = e.gen_focus == c
let take_gen_focus e c = e.gen_focus <- c
let lose_gen_focus e c = e.gen_focus <- empty_component

let rec send_event rs c =
   match get_event rs with
     MouseDown | MouseUp | MouseDrag | MouseMove ->
      if c.mem(get_mouse_x rs, get_mouse_y rs) then
        if  List.exists (fun sun -> send_event rs sun) c.children then true
        else ( if c.listener rs then (rs.last <-c; true) else false )
      else false
   | KeyPress | KeyRelease ->
      if has_key_focus rs c then
        ( if c.listener rs then (rs.last<-c; true)
          else false )
      else List.exists (fun sun -> send_event rs sun) c.children
   | _  -> c.listener rs

let compute_rich_event s0 s1  =
  if s0.Graphics.button <> s1.Graphics.button then
  begin
    if s0.Graphics.button then MouseDown else MouseUp
  end
  else if s1.Graphics.keypressed then KeyPress
  else if (s0.Graphics.mouse_x <> s1.Graphics.mouse_x  ) ||
          (s0.Graphics.mouse_y <> s1.Graphics.mouse_y  ) then
  begin
    if s1.Graphics.button then MouseDrag else MouseMove
  end
  else raise Not_found

let send_new_events res0 res1 =
  if res0.key_focus <> res1.key_focus then
    begin
      ignore(send_event  {res1 with re = LostFocus} res0.key_focus);
      ignore(send_event  {res1 with re = GotFocus} res1.key_focus)
    end;
  if (res0.last <> res1.last) &&
     (( res1.re = MouseMove) || (res1.re = MouseDrag)) then
    begin
      ignore(send_event  {res1 with re = MouseExit} res0.last);
      ignore(send_event  {res1 with re = MouseEnter} res1.last )
    end

let courier_bold_24 = Sopt "*courier-bold-r-normal-*24*"
and courier_bold_18  = Sopt "*courier-bold-r-normal-*18*"

let initial_re =
  { re = Exposure;
    stat = { Graphics.mouse_x=0; Graphics.mouse_y=0;
             Graphics.key = ' ';
             Graphics.button = false;
             Graphics.keypressed = false };
    key_focus = empty_component;
    gen_focus = empty_component;
    last = empty_component }

let display_init c =
   Graphics.set_color (get_gc_bcol (get_gc c)); display_rect  c ();
   let gc= get_gc c in
     use_gc gc;
     let (a,b) = get_gc_cur gc in
       Graphics.moveto (c.x+a) (c.y+b)
  let display_label s c () =
     display_init c; Graphics.draw_string s
(* ===============Button======= *)
type button_state =
  { txt : string; mutable action :  button_state -> unit }

let create_bs s = {txt = s; action = fun x -> ()}
 let set_bs_action bs f = bs.action <- f
 let get_bs_text bs = bs.txt

let display_button  c bs  () =
   display_init c;  Graphics.draw_string (get_bs_text bs)
let listener_button c bs  e = match get_event e with
  MouseDown -> bs.action bs; c.display (); true
  | _ -> false

let create_panel b  w h  lopt =
   let u = create_component w h   in
     u.container <- b;
     u.info <- if b then "Panel container" else "Panel";
     let gc = make_default_context () in set_gc gc lopt; u.gc <- gc;
     u

let center_layout c c1 lopt =
  c1.x <- c.x + ((c.w -c1.w) /2); c1.y <- c.y + ((c.h -c1.h) /2)

let grid_layout (a, b)  c c1 lopt =
   let px = theInt lopt "Col" 0
   and py = theInt lopt "Row" 0 in
    if (px >= 0) && (px < a) && ( py >=0) && (py < b) then
      let lw = c.w /a
      and lh = c.h /b in
           if (c1.w > lw) || (c1.h > lh) then
             failwith "grid_placement: too big component"
           else
             c1.x <- c.x + px * lw + (lw - c1.w)/2;
             c1.y <- c.y + py * lh + (lh - c1.h)/2;
    else  failwith "grid_placement: bad position"


let create_button s lopt     =
   let bs = create_bs s in
     let gc = make_default_context () in
       set_gc gc lopt; use_gc gc;
       let w,h = Graphics.text_size (get_bs_text bs) in
         let u = create_component w h   in
           u.display <- display_button u bs;
           u.listener <- listener_button u bs;
           u.info <- "Button";
           u.gc <- gc;
           u,bs

(* let b,bs = create_button "Validation" ["Font", courier_bold_24;
                                        "Background", Copt gray1]
let p2 = create_panel true 150 60 ["Background", Copt gray2]
set_bs_action bs (fun bs -> print_string ( (get_bs_text bs)^ "...");
                     print_newline())
set_layout  (center_layout p2) p2
   add_component p2 b [] *)

type textfield_state =
  { txt : string;
    dir : bool; mutable ind1 : int; mutable ind2 : int; len : int;
    mutable visible_cursor : bool; mutable cursor : char;
    mutable visible_echo : bool; mutable echo : char;
    mutable action : textfield_state -> unit }

let create_tfs txt size dir  =
  let l = String.length txt in
  (if size < l then failwith "create_tfs");
  let ind1 = if dir then 0 else size-1-l
  and ind2 = if dir then l else size-1 in
  let n_txt = (if dir then (txt^(String.make (size-l) ' '))
               else ((String.make (size-l) ' ')^txt )) in
  {txt = n_txt; dir=dir; ind1 = ind1; ind2 = ind2; len=size;
   visible_cursor  = false;  cursor = ' '; visible_echo =  true; echo = ' ';
   action= fun x -> ()}

let set_tfs_action tfs f = tfs.action <- f
let set_tfs_cursor b c tfs =  tfs.visible_cursor <- b; tfs.cursor <- c
let set_tfs_echo b c tfs =  tfs.visible_echo <- b; tfs.echo <- c
let get_tfs_text tfs =
  if tfs.dir then String.sub tfs.txt tfs.ind1 (tfs.ind2 - tfs.ind1)
  else String.sub tfs.txt (tfs.ind1+1) (tfs.ind2 - tfs.ind1)

let set_tfs_text tf tfs txt =
  let l = String.length txt in
    if l > tfs.len then failwith "set_tfs_text";
    (* Bytes.blit_string  (String.make tfs.len ' ') 0 tfs.txt 0 tfs.len; *)
    (* let _ = String.sub (String.make tfs.len ' ') 0 tfs.len in in ();*)
    if tfs.dir then (Bytes.blit_string txt 0 tfs.txt 0 l;
                   tfs.ind2 <- l )
    else   ( Bytes.blit_string txt 0 tfs.txt (tfs.len -l) l;
           tfs.ind1 <- tfs.len-l-1 );
  tf.display ()

let display_cursor c tfs =
  if tfs.visible_cursor then
    ( use_gc (get_gc c);
      let (x,y) = Graphics.current_point() in
      let (a,b) = Graphics.text_size " " in
      let shift =  a *  (if tfs.dir then max (min (tfs.len-1) tfs.ind2)  0
                         else tfs.ind2) in
      Graphics.moveto (c.x+x + shift) (c.y+y);
      Graphics.draw_char tfs.cursor)

let display_textfield c tfs  () =
    display_init c;
    let s = String.make tfs.len ' '
    and txt = get_tfs_text tfs in
      let nl = String.length txt in
      if (tfs.ind1 >= 0) && (not tfs.dir) then
         Graphics.draw_string (String.sub s 0 (tfs.ind1+1) );
      if tfs.visible_echo  then (Graphics.draw_string (get_tfs_text tfs))
      else Graphics.draw_string (String.make (String.length txt) tfs.echo);
      if (nl > tfs.ind2) && (tfs.dir)
        then Graphics.draw_string (String.sub s tfs.ind2 (nl-tfs.ind2));
      display_cursor c tfs


let listener_text_field u tfs e =
     match e.re with
       MouseDown -> take_key_focus e u ; true
     | KeyPress ->
         ( if Char.code (get_key e)  >= 32 then
           begin
             ( if tfs.dir then
               ( ( if tfs.ind2 >= tfs.len then (
                     Bytes.blit_string tfs.txt 1 tfs.txt 0 (tfs.ind2-1);
                     tfs.ind2 <- tfs.ind2-1) );
                   tfs.txt.[tfs.ind2] <- get_key e;
                   tfs.ind2 <- tfs.ind2 +1 )
               else
               ( Bytes.blit_string tfs.txt 1 tfs.txt 0 (tfs.ind2);
                 tfs.txt.[tfs.ind2] <- get_key e;
                 if tfs.ind1 >= 0 then tfs.ind1 <- tfs.ind1 -1
               );
             )
           end
           else (
           ( match Char.code (get_key e) with
               13 -> tfs.action tfs
             |  9 -> lose_key_focus e u
             |  8 -> if (tfs.dir && (tfs.ind2 > 0))
                       then tfs.ind2 <- tfs.ind2 -1
                       else if (not tfs.dir) && (tfs.ind1 < tfs.len -1)
                            then tfs.ind1 <- tfs.ind1+1
             | _ -> ()
           ))); u.display(); true
     | _ -> false


let create_text_field  txt size dir lopt  =
  let tfs = create_tfs txt size dir
  and l = String.length txt in
   let gc = make_default_context () in
    set_gc gc lopt; use_gc gc;
    let (w,h) = Graphics.text_size (tfs.txt) in
      let u = create_component w h   in
        u.display <- display_textfield u tfs;
        u.listener <-  listener_text_field u tfs ;
        u.info <- "TextField"; u.gc <- gc;
        u,tfs
