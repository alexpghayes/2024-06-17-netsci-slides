

Thanks for all this. It’ll take me a few days to digest the paper. But my instinct is that trying to interpret latent factors is fraught.

 

In the smoking example, I wonder if the story could go something like:

    Problem: I want to know the effect of sex on smoking
    Solution: I do an obs study and look at E[Y_i(1) – Y_i(0)] where that expectation is taken over the finite-population
    Problem with this naïve study: smoking has a social component and the outcome can be highly dependent across units; that is (Y_i(1), Y_i(0)) may be dependent on (Y_j(1), Y_j(0)) if i and j are friends.
    Solution: so we need to account for the adjacency information. A naïve way to do this would be to make the usual causal assumptions and do a regression but make the error terms dependent (e.g., their precision matrix is the Laplacian of the network, a la Besag’s conditional/intrinsic autoregressive models)
    Problem: But friendship could be affected by sex! So if I (hypothetically) intervene on the sex of i, they could potentially no longer become friends with j. In essence, the dependence of the observables Y_i and Y_j can vary with treatments (z_i, z_j). 

 

This is just a rough sketch but I think you can go from here to say in a larger DAG (in which we have nodes for every individual; not the usual convention of assuming we have a single DAG and n iid observations from it) we have a direct & indirect path from z_i to observable Y_i. In some sense, I’m suggesting being much less abrupt in your introduction to direct/indirect effects. That is, instead of saying “I’m doing mediation but with networks” you could say “I’m analyzing network data and I’ll argue that we need to take an approach that looks (and walk and quacks) like mediation analysis”.

 