module G = Graphics ;;
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
module Utilities = Utilities ;; 

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect
                  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Registry.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Registry.register (self :> thing_type)

    method update : unit =
      self#move
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states
  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump
                     
    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
	  (List.map (fun obj -> obj#infectiousness)
                    (Registry.neighbors (self :> thing_type))) in
      (*Vaccinate given proportion of individuals*)
      if Utilities.flip_coin cVACCINATED then 
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new vaccinated posx posy) :> thing_type)
        end
      (* if infected, update the registry by replacing this object
         with an infected one *)
      else if Utilities.flip_coin infectiousness_total then
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new infected posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
            as super

    initializer
      Stat.infected#bump

    (*.................................................................
      Place any augmentations to `infected` here.
    ................................................................ *)

    val mutable recovery : float = let mean, std_dev = cIMMUNITY_PERIOD in 
                                   Utilities.gaussian mean std_dev

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_INFECTED;
      Viz.draw_circle ~size:(cNEIGHBOR_RADIUS + cSYMBOL_SIZE) ~filled:false x y cCOLOR_INFECTED

    method! update =
      super#update;
      let posx, posy = self#pos in
      if recovery <= 0. then
        begin
          Stat.infected#debump;
          Registry.deregister (self :> thing_type);
          (* Turn into zombie with given probability *)
          if Utilities.flip_coin cZOMBIE then
            Registry.register ((new zombie posx posy) :> thing_type)
          (* Die with given probability *)
          else if Utilities.flip_coin cMORTALITY then
            Registry.register ((new deceased posx posy) :> thing_type)
          (* Recover otherwise *)
          else
            Registry.register ((new recovered posx posy) :> thing_type)
        end
      else
          recovery <- recovery -. 1.

  end

and (* class *) recovered (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_RECOVERED
                   cINFECTIOUSNESS_RECOVERED
            as super

    initializer
      Stat.recovered#bump

    val mutable immunity : float = let mean, std_dev = cIMMUNITY_PERIOD in 
                                   Utilities.gaussian mean std_dev

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_RECOVERED

    method! update =
      super#update;
      let posx, posy = self#pos in
      (* Once immunity period is up, become susceptible again *)
      if immunity <= 0. then
        begin
          Stat.recovered#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new susceptible posx posy) :> thing_type)
        end
      else
        immunity <- immunity -. 1.
    end

and (* class *) deceased (initx : int) (inity : int) =
    object (self)
    inherit person initx inity
                   cSTEP_SIZE_DECEASED
                   cINFECTIOUSNESS_DECEASED
            as super

    initializer
      Stat.deceased#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_DECEASED

    method! update =
      super#update
  end

(* Vaccinated individuals remain immune *)
and (* class *) vaccinated (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_VACCINATED
                   cINFECTIOUSNESS_VACCINATED
            as super

    initializer
      Stat.vaccinated#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_VACCINATED

    method! update =
      super#update
  end

(* Zombies can move and infect susceptible individuals, but always remain zombies *)
and (* class *) zombie (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_ZOMBIE
                   cINFECTIOUSNESS_ZOMBIE
            as super

    initializer
      Stat.zombie#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_ZOMBIE;
      Viz.draw_circle ~size:(cNEIGHBOR_RADIUS + cSYMBOL_SIZE) ~filled:false x y cCOLOR_ZOMBIE

    method! update =
      super#update
  end
;;
