
***
This information on blue/green colour terms is coming from CLICS website (Rzymski et al., 2020)
***

We first gathered all languages having a concept for blue (https://clics.clld.org/parameters/837#1/21/1).
Then, we gatherered all population for which the blue concept is colexified with green (https://clics.clld.org/edges/837-1425) or another colour.
This information is shown in the column colexification: 
- "none" if the concept blue is not colexified
- "green" if the concept is colexified with the green concept
- "green black" for example if the concept is colexified with green and black concepts

Then, using this information, we created the column "exists_blue":
- If the blue concept is colexified with green, the variable "exists_blue" is set to "no".
- If the blue concept is **not** colexified with green, the variable "exists_blue" is set to "yes".
- If the blue concept is colexified with green and another colour, the variable "exists_blue" is set to "no".
- If the blue concept is colexified with another colour but not green, the variable "exists_blue" is set to "mixed_with".

All meta data (glottocode, location) were extracted from CLICS database (see https://clics.clld.org/download).


