open Js_of_ocaml
open Js_of_ocaml_lwt
open Js

(* ========= constants ========== *)
let max_depth = 6
let min_depth = 1
let delta_zoom = 0.04
let delta_base_move = 0.03
let init_upleft_lon = -76.5527
let init_upleft_lat = 42.4883
let init_lowright_lon = -76.4649
let init_lowright_lat = 42.4235


type params = {
  upleft_lon: float;
  upleft_lat: float;
  lowright_lon: float;
  lowright_lat: float;
  width: float;
  height: float;
}
(* TODO *)
type gui_state = {
  params: params;
  current_depth: int;
}



(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

module Html = Dom_html
let js = Js.string
let doc = Html.document

let countries = ["Afghanistan";"Albania";"Algeria";"Andorra";"Angola";"Anguilla";"Antigua & Barbuda";"Argentina";"Armenia";"Aruba";"Australia";"Austria";"Azerbaijan";"Bahamas";"Bahrain";"Bangladesh";"Barbados";"Belarus";"Belgium";"Belize";"Benin";"Bermuda";"Bhutan";"Bolivia";"Bosnia & Herzegovina";"Botswana";"Brazil";"British Virgin Islands";"Brunei";"Bulgaria";"Burkina Faso";"Burundi";"Cambodia";"Cameroon";"Canada";"Cape Verde";"Cayman Islands";"Central Arfrican Republic";"Chad";"Chile";"China";"Colombia";"Congo";"Cook Islands";"Costa Rica";"Cote D Ivoire";"Croatia";"Cuba";"Curacao";"Cyprus";"Czech Republic";"Denmark";"Djibouti";"Dominica";"Dominican Republic";"Ecuador";"Egypt";"El Salvador";"Equatorial Guinea";"Eritrea";"Estonia";"Ethiopia";"Falkland Islands";"Faroe Islands";"Fiji";"Finland";"France";"French Polynesia";"French West Indies";"Gabon";"Gambia";"Georgia";"Germany";"Ghana";"Gibraltar";"Greece";"Greenland";"Grenada";"Guam";"Guatemala";"Guernsey";"Guinea";"Guinea Bissau";"Guyana";"Haiti";"Honduras";"Hong Kong";"Hungary";"Iceland";"India";"Indonesia";"Iran";"Iraq";"Ireland";"Isle of Man";"Israel";"Italy";"Jamaica";"Japan";"Jersey";"Jordan";"Kazakhstan";"Kenya";"Kiribati";"Kosovo";"Kuwait";"Kyrgyzstan";"Laos";"Latvia";"Lebanon";"Lesotho";"Liberia";"Libya";"Liechtenstein";"Lithuania";"Luxembourg";"Macau";"Macedonia";"Madagascar";"Malawi";"Malaysia";"Maldives";"Mali";"Malta";"Marshall Islands";"Mauritania";"Mauritius";"Mexico";"Micronesia";"Moldova";"Monaco";"Mongolia";"Montenegro";"Montserrat";"Morocco";"Mozambique";"Myanmar";"Namibia";"Nauro";"Nepal";"Netherlands";"Netherlands Antilles";"New Caledonia";"New Zealand";"Nicaragua";"Niger";"Nigeria";"North Korea";"Norway";"Oman";"Pakistan";"Palau";"Palestine";"Panama";"Papua New Guinea";"Paraguay";"Peru";"Philippines";"Poland";"Portugal";"Puerto Rico";"Qatar";"Reunion";"Romania";"Russia";"Rwanda";"Saint Pierre & Miquelon";"Samoa";"San Marino";"Sao Tome and Principe";"Saudi Arabia";"Senegal";"Serbia";"Seychelles";"Sierra Leone";"Singapore";"Slovakia";"Slovenia";"Solomon Islands";"Somalia";"South Africa";"South Korea";"South Sudan";"Spain";"Sri Lanka";"St Kitts & Nevis";"St Lucia";"St Vincent";"Sudan";"Suriname";"Swaziland";"Sweden";"Switzerland";"Syria";"Taiwan";"Tajikistan";"Tanzania";"Thailand";"Timor L'Este";"Togo";"Tonga";"Trinidad & Tobago";"Tunisia";"Turkey";"Turkmenistan";"Turks & Caicos";"Tuvalu";"Uganda";"Ukraine";"United Arab Emirates";"United Kingdom";"United States of America";"Uruguay";"Uzbekistan";"Vanuatu";"Vatican City";"Venezuela";"Vietnam";"Virgin Islands (US)";"Yemen";"Zambia";"Zimbabwe"]

(* Set the class of an Html element *)
let setClass elt s = elt##className <- js s
(* Set the ID of an Html element *)
let setId elt s = elt##id <- js s

let append_text e s = Dom.appendChild e (doc##createTextNode (js s))

(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail


(* close all autocomplete lists in the document*)
    let closeAllList elt input =
      Js.Opt.iter input (fun inp ->
          match elt with
          | None -> let element =  (Html.createDiv doc) in
            let x = doc##getElementsByClassName (js "autocomplete-items") in
            for i = 0 to x##length - 1 do
              Js.Opt.iter (x##item (i))
                (fun i ->
                   if(element <> i && element <> inp)
                   then Js.Opt.iter (i##parentNode) (fun x -> Dom.removeChild x i))
            done
          | Some element ->
            let x = doc##getElementsByClassName (js "autocomplete-items") in
            for i = 0 to x##length - 1 do
              Js.Opt.iter (x##item (i))
                (fun i -> if(element <> i && element <> inp)
              then Js.Opt.iter (i##parentNode) (fun x -> Dom.removeChild x i))
        done
    )

(* onload _ loads all the required HTML elements upon GUI launching *)
let onload _ =
  let img_dest = Html.createImg doc in
  setId img_dest "dest";
  img_dest##src <- js "marker.gif";
  Dom.appendChild doc##body img_dest;
  (* ==================== begin div map-container ==================== *)

  let div_map_container = Html.createDiv doc in
  setClass div_map_container "map-container";
  Dom.appendChild doc##body div_map_container;
  (* append_text div_map_container "Loading.."; *)

  let div_mapbody = Html.createDiv doc in
  setClass div_mapbody "mapbody";
  Dom.appendChild div_map_container div_mapbody;
  (* append_text div_mapbody "Hi"; *)
  div_mapbody##ondblclick <- Dom_html.handler
      (fun _ ->
         img_dest##style##visibility <- js "visible";
         img_dest##style##transform <- js "translateX(500px)translateY(500px)";Js._true);

  let img_map = Html.createImg doc in
  setId img_map "map";
  img_map##src <- js "../tiles/1.png";
  Dom.appendChild div_mapbody img_map;

  (* ==================== end div map-container ==================== *)


  let div_markers = Html.createDiv doc in
  setId div_markers "markers";
  Dom.appendChild doc##body div_markers;

  let div_actions = Html.createDiv doc in
  setClass div_actions "actions";
  Dom.appendChild doc##body div_actions;

  (* let div_widget_card = Html.createDiv doc in
     setClass div_widget_card "widget card";
     Dom.appendChild div_actions div_widget_card; *)

  let div_card_content = Html.createDiv doc in
  setClass div_card_content "card-content";
  Dom.appendChild div_actions div_card_content;

  let div_autocomplete = Html.createDiv doc in
  setClass div_autocomplete "autocomplete";
  Dom.appendChild div_card_content div_autocomplete;

  let div_bottom = Html.createDiv doc in
  setClass div_bottom "bottom";
  Dom.appendChild div_autocomplete div_bottom;

  let input_1 = Html.createInput doc in
  setId input_1 "input1";
  input_1##placeholder <- js "Start Location";
  Dom.appendChild div_bottom input_1;

  let input_2 = Html.createInput doc in
  setId input_2 "input2";
  input_2##placeholder <- js "Destination";
  Dom.appendChild div_autocomplete input_2;
  (* let span_search_container = Html.createSpan doc in
     setClass span_search_container "search-container";
     Dom.appendChild div_card_content span_search_container;

     let label_for_tags = Html.createLabel doc in
     label_for_tags##htmlFor <- js "tags";
     Dom.appendChild span_search_container label_for_tags;

     let input_1 = Html.createInput doc in
     setClass input_1 "search";
     setId input_1 "tags";
     input_1##placeholder <- js "Search locations";
     Dom.appendChild span_search_container input_1; *)

  (* ==================== begin icons ==================== *)

  let span_icons_container = Html.createSpan doc in
  setClass span_icons_container "icons-container";
  Dom.appendChild div_card_content span_icons_container;
  let a_zoomin = Html.createA doc in
  setClass a_zoomin "zoomin";
  let i_plus = Html.createI doc in
  setClass i_plus "action-icon fa fa-2x fa-search-plus";
  Dom.appendChild a_zoomin i_plus;


  let a_zoomout = Html.createA doc in
  setClass a_zoomout "zoomout";
  let i_minus = Html.createI doc in
  setClass i_minus "action-icon fa fa-2x fa-search-minus";
  Dom.appendChild a_zoomout i_minus;


  let a_info = Html.createA doc in
  setClass a_info "info";

  let i_question = Html.createI doc in
  setClass i_question "action-icon fa fa-2x fa-question-circle";
  Dom.appendChild a_info i_question;

  let div_info_text = Html.createDiv doc in
  setClass div_info_text "info-text";
  append_text div_info_text "You can also use arrow keys and -/= to zoom, or use the mouse drag and scroll wheel.
  Double click to begin routing & double click again to end and show route.";
  Dom.appendChild a_info div_info_text;

  let div_info_subtext = Html.createDiv doc in
  setClass div_info_subtext "info-subtext";
  append_text div_info_subtext "( click this bubble to close )";
  Dom.appendChild div_info_text div_info_subtext;

  Dom.appendChild span_icons_container a_zoomin;
  Dom.appendChild span_icons_container a_zoomout;
  Dom.appendChild span_icons_container a_info;

  (* When the question mark icon is clicked, show the info subtext *)
  a_info##onclick <- Dom_html.handler
      (fun _ ->
         if div_info_text##style##display = Js.string "none" then
           (div_info_text##style##display <- Js.string "block";Js._true)
         else
           (div_info_text##style##display <- Js.string "none";Js._true));

  (* ==================== end icons ==================== *)

  let div_nothing = Html.createDiv doc in
  Dom.appendChild div_actions div_nothing;

  (* Clear the text in the textbox when "Clear Route" is clicked *)
  let a_clear = Html.createA doc in
  setClass a_clear "clear waves-effect btn";
  append_text a_clear "clear route";
  a_clear##onclick <- Html.handler
      (fun _ ->
         input_1##value <- js "";
         Js._true);
  Dom.appendChild div_nothing a_clear;



  (* the autocompletion functionality *)
  let currentFocus = ref 0 in
  input_1##oninput <- Html.handler
    (fun _ ->
      doc##onclick <- Html.handler (fun ev -> (closeAllList (Js.Opt.to_option ev##target) (Dom_html.CoerceTo.element input_1));Js._true);
      (* Create a new div to contain all the relevant autocomplete item *)
      let a = Html.createDiv doc in
      let v = Js.to_string input_1##value in
      closeAllList None (Dom_html.CoerceTo.element input_1);
      currentFocus := -1;

         (* let newDiv = Html.createDiv doc in *)
      setId a (Js.to_string input_1##id^ "autocomplete-list");
      setClass a "autocomplete-items";
      (match Js.Opt.to_option input_1##parentNode with
      | None -> failwith "error"
      | Some x -> Dom.appendChild x a);
      (* Dom.appendChild div_card_content a; *)
      for i = 0 to List.length countries - 1 do
      let word = List.nth countries i in
      if(String.(sub word 0 (length v) |> uppercase_ascii) = String.uppercase_ascii v)
      (* create a DIV element for each matching element: *)
      then let b = ref (Html.createDiv doc) in
      (* make the matching letters bold: *)
      let inn = "<strong>" ^ (String.sub word 0 (String.length v)) ^ "</strong>" ^
        (String.sub word (String.length v) (String.length word-String.length v))^
        "<input type='hidden' value='" ^ word ^ "'>" in
        !b##innerHTML <- js inn;
        (* execute a function when someone clicks on the item value (DIV element): *)
        (* When one of the suggested text is clicked, change the input to that word. *)
        !b##onclick <- Html.handler
            (fun _ ->
            (* returns a Dom.nodeList *)
            let inputfield = (!b)##getElementsByTagName (js "input") in
            (* the first item of the list *)
            let firstone = inputfield##item (0) in
            Js.Opt.iter firstone
              (fun node ->
                let elt = Dom_html.CoerceTo.element node in
                Js.Opt.iter elt
                (fun elt ->
                  let input = Dom_html.CoerceTo.input elt in
                  Js.Opt.iter input
                  (fun i ->
                    (* change the value in the textbox to the clicked text *)
                    let content = i##value in
                    input_1##value <- content

                  )
                )
            )
            ;Js._true
          );
           (* ((page##(getElementsByTagName (Js.string "head")))##(item (0))) *)
      Dom.appendChild a !b
             (* Js.Opt.iter (childNodes##item i) *)
             (* (fun node -> node##classList##remove (js "autocomplete-active")) *)
      done;
      Js._true);
  Js._false

(* Start to load the page *)
let () =
  Dom_html.window##onload <- Dom_html.handler onload
