###############################################################
# producing input to coco's sub.exe routine
###############################################################
#
# the heavy lifting behind producing valid input to sub.exe
#
Btonrs := function(f,B,n,pairs)
   local M, i, j, k, p, nc;
   Print("\n n=", n, " pairs=", pairs);
   PrintTo(f,"1\n",n,"\n");
   if pairs=() then 
       AppendTo(f,"\n");
   else
       AppendTo(f,pairs,"\n");
   fi;
   nc := 0;
   for i in [1..n] do
      M := B(i);
      for j in [1..n] do 
         for k in [1..n] do 
           p := M[k][j];
           if p>0 then 
              nc := nc+1;
           fi;
   od; od; od;

   AppendTo(f,nc,"\n");
   for i in [1..n] do
      M := B(i);
      for j in [1..n] do 
         for k in [1..n] do 
           p := M[j][k];
           if p>0 then 
              nc := nc-1;
              AppendTo(f,"(",i-1,",",j-1,",",k-1,"-",p,")");
              if nc>0 then
                  AppendTo(f,"*\n");
              else
                  AppendTo(f,"\n");
              fi;
           fi;
   od; od; od;
   AppendTo(f,"\n"); # to make the input readable by sub.exe directly
end;

#
# constructing the multiplication coefficients (a.k.a. structure constants) file
# for COCO from the character table of a group
# 
tabletonrs := function(t,f)
   local MC, n, i, p;
   n := Length(Irr(t));
   MC := function(i)
      return MatClassMultCoeffsCharTable(t,i);
   end;
   p := PermList(List(InverseClasses(t){[2..n]},i->i-1));
   Btonrs(f,MC,n,p);
end;

#
# constructing the multiplication coefficients (a.k.a. structure constants) file
# for COCO from the collapsed orbital matrices of a 
# transitive permutation  group t
# 
LoadPackage("grape");
coladjmatstonrs := function(t,f)
   local MC, M, n, i, p, pairs;
   M := OrbitalGraphColadjMats(t);
   n := Length(M[1]);
   MC := function(i)
      return M[i]; 
      #return TransposedMat(M[i]); 
   end;
   p:=[1..n];
   for i in [1..n] do
      if M[i][1][i]=0 then
        Error("unexpected ordering!");
      fi;
      p[i]:=Position(TransposedMat(M[i])[1],1);
   od;
   pairs := PermList(List(p{[2..n]},i->i-1));
   Btonrs(f,MC,n, pairs);
end;

Gtogen := function(f,G,D)
   local c, j, gg, ng, n, g, cc, lcc, lc;
   gg := GeneratorsOfGroup(G);
   ng := Length(gg);
   n := Length(D);
   PrintTo(f,n,"\n",ng,"\n");
   
   for g in gg do
      cc := Cycles(g,D);
      lcc := Length(cc);
      for c in cc do
         lcc := lcc - 1;
         lc := Length(c);
	 if lc>0 then
	  AppendTo(f,"(");
	  for j in c do
	     AppendTo(f,j-1);
             lc := lc - 1;
	     if lc>0 then 
	        AppendTo(f,",");
	     fi;
          od;
	  AppendTo(f,")");
	  if lcc>0 then 
	     AppendTo(f,"*\n");
          fi;
	 fi;
      od;
      AppendTo(f,"\n");
   od;
end;
