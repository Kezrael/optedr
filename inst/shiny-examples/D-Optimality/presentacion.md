Presentation
------------

This shinny application presents a tool to calculate optimal designs for
Antoine’s Equation given different initial estimations of the unknown
parameters, *θ*<sup>(0)</sup>, and design space, 𝒳.

This application uses the *Cocktail Algorithm* to calculate optimal
designs for Antoine’s Equation. The details of the algorithm can be
found in the original work accompaning this application.

The functioning of the different tabs of the app are as follows

-   <u>Presentation</u>: description on how the app works.
-   Optimal design: calculates *D*−, *D*<sub>*s*</sub>, *A*− and
    *I*−optimal designs for Antoine’s Equation. The user decides the
    criterion and the associated parameters by either manually selecting
    them or choosing a substance from the sample list included. Then,
    after doing extra choices if necessary, the user calculates the
    optimal design by clicking *Calculate Optimal Design*.
-   Custom design: The Custom Design menu lets the user create his own
    design, and compare the efficiency of their design with the optimum
    design calculated before.
-   Efficiency of industry design: lets you calculate the efficiency of
    some commonly used designs for different processes:
    -   Equidistant: Designs supported in both extremes of the space of
        the design, with *n* points that are equidistant between them
        and uniform weights.
    -   Arithmetic: Designs supported in the one of the extremes (top)
        of the space of the design, with *n* points that follow an
        arithmetic progression and have uniform weights, and the
        leftmost point is choose in a way that optimizes the efficiency
        of the resulting design.
    -   Geometric: Designs supported in the one of the extremes (top) of
        the space of the design, with *n* points that follow a geometric
        progression and have uniform weights, and the leftmost point is
        choose in a way that optimizes the efficiency of the resulting
        design.
-   Contact: information for contacting the authors of the app.

**NOTE**: the inputs for the user to choose are on the left sidebar,
while the main body of the app is used only to reflect the outputs.

**NOTE**: for some set of parameters the Information Matrix might be bad
conditioned, which causes the inverse calculating function to fail, and
hence an error is promped to the user.
