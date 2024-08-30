functions {
// spatial functions from : https://github.com/ConnorDonegan/Stan-IAR
/**
* Log probability of the intrinsic conditional autoregressive (ICAR) prior,
* excluding additive constants. 
*
* @param phi Vector of parameters for spatial smoothing (on unit scale)
* @param spatial_scale Scale parameter for the ICAR model
* @param node1 
* @param node2
* @param k number of groups
* @param group_size number of observational units in each group
* @param group_idx index of observations in order of their group membership
* @param has_theta If the model contains an independent partial pooling term, phi for singletons can be zeroed out; otherwise, they require a standard normal prior. Both BYM and BYM2 have theta.
*
* @return Log probability density of ICAR prior up to additive constant
**/
  real icar_normal_lpdf(vector phi, real spatial_scale,
                        int[] node1, int[] node2, 
                        int k, int[] group_size, int[] group_idx,
                        int has_theta) {
    real lp;
    int pos=1;
    lp = -0.5 * dot_self(phi[node1] - phi[node2]);
    if (has_theta) {
      for (j in 1:k) {
        /* sum to zero constraint for each connected group; singletons zero out */
          lp += normal_lpdf(sum(phi[segment(group_idx, pos, group_size[j])]) | 0, 0.001 * group_size[j]);
          pos += group_size[j];
      }
    } else {
      /* does not have theta */
        for (j in 1:k) {
          if (group_size[j] > 1) {
            /* same as above for non-singletons: sum to zero constraint */
              lp += normal_lpdf(sum(phi[segment(group_idx, pos, group_size[j])]) | 0, 0.001 * group_size[j]);
          } else {
            /* its a singleton: independent Gaussian prior on phi */
              lp += normal_lpdf(phi[ segment(group_idx, pos, group_size[j]) ] | 0, spatial_scale);
          }      
          pos += group_size[j];
        }
    }
    return lp;
  }

/**
* Combine local and global partial-pooling components into the convolved BYM term.
*
* @param phi local (spatially autocorrelated) component (not phi_tilde!)
* @param theta global component (not theta_tilde!)
* @param n number of spatial units
* @param k number of connected groups
* @param group_size number of observational units in each group
* @param group_idx index of observations in order of their group membership
*
* @return BYM convolution vector
*/
  vector convolve_bym(vector phi, vector theta,
                      int n, int k,
                      int[] group_size, int[] group_idx
  ) {
    vector[n] convolution;
    int pos=1;
    for (j in 1:k) {
      if (group_size[j] == 1) {
        convolution[ segment(group_idx, pos, group_size[j]) ] = theta[ segment(group_idx, pos, group_size[j]) ];
      } else {
        convolution[ segment(group_idx, pos, group_size[j]) ] =
          phi[ segment(group_idx, pos, group_size[j]) ] + theta[ segment(group_idx, pos, group_size[j]) ];
      }
      pos += group_size[j];
    }
    return convolution;
  }


/**
* Combine local and global partial-pooling components into the convolved BYM2 term.
*
* @param phi_tilde local (spatially autocorrelated) component
* @param theta_tilde global component
* @param spatial_scale scale parameter for the convolution term
* @param n number of spatial units
* @param k number of connected groups
* @param group_size number of observational units in each group
* @param group_idx index of observations in order of their group membership
* @param inv_sqrt_scale_factor The scaling factor for the ICAR variance (see scale_c R function, using R-INLA); 
*                              transformed from 1/scale^2 --> scale. Or, a vector of ones.
* @param rho proportion of convolution that is spatially autocorrelated
*
* @return BYM2 convolution vector
*/
  vector convolve_bym2(vector phi_tilde, vector theta_tilde,
                       real spatial_scale,
                       int n, int k,
                       int[] group_size, int[] group_idx,
                       real rho, vector inv_sqrt_scale_factor
  ) {
    vector[n] convolution;
    int pos=1;
    for (j in 1:k) {
      if (group_size[j] == 1) {
        convolution[ segment(group_idx, pos, group_size[j]) ] = spatial_scale * theta_tilde[ segment(group_idx, pos, group_size[j]) ];
      } else {
        convolution[ segment(group_idx, pos, group_size[j]) ] = spatial_scale * (
          sqrt(rho) * inv_sqrt_scale_factor[j] * phi_tilde[ segment(group_idx, pos, group_size[j]) ] +
            sqrt(1 - rho) * theta_tilde[ segment(group_idx, pos, group_size[j]) ]
        );
      }
      pos += group_size[j];
    }
    return convolution;
  }


/**
  * Create phi from phi_tilde, inv_sqrt_scale_factor, and spatial_scale. 
*
  * @param phi_tilde local component (spatially autocorrelated) 
* @param phi_scale scale parameter for ICAR Normal model
* @param inv_sqrt_scale_factor The scaling factor for the ICAR variance (see scale_c R function, using R-INLA); 
*                              transformed from 1/scale^2 --> scale. Or, a vector of ones.
* @param n number of spatial units
* @param k number of connected groups
* @param group_size number of observational units in each group
* @param group_idx index of observations in order of their group membership
*
  * @return phi vector of spatially autocorrelated coefficients
*/
  vector make_phi(vector phi_tilde, real phi_scale,
                  vector inv_sqrt_scale_factor,
                  int n, int k,
                  int[] group_size, int[] group_idx
  ) {
    vector[n] phi;
    int pos=1;
    for (j in 1:k) {
      phi[ segment(group_idx, pos, group_size[j]) ] = phi_scale * inv_sqrt_scale_factor[j] * phi_tilde[ segment(group_idx, pos, group_size[j]) ];
      pos += group_size[j];
    }
    return phi;
  }



/**
  * Create phi from phi_tilde, inv_sqrt_scale_factor, and spatial_scale: 
  *  with separate scale parameters per component.
*
  * @param phi_tilde local component (spatially autocorrelated) 
* @param phi_scale scale parameters for ICAR Normal model, per connected component
* @param inv_sqrt_scale_factor The scaling factor for the ICAR variance (see scale_c R function, using R-INLA); 
*                              transformed from 1/scale^2 --> scale. Or, a vector of ones.
* @param n number of spatial units
* @param k number of connected groups
* @param group_size number of observational units in each group
* @param group_idx index of observations in order of their group membership
*
  * @return phi vector of spatially autocorrelated coefficients
*/
  vector make_phi2(vector phi_tilde, vector phi_scale,
                   vector inv_sqrt_scale_factor,
                   int n, int k,
                   int[] group_size, int[] group_idx
  ) {
    vector[n] phi;
    int pos=1;
    for (j in 1:k) {
      phi[ segment(group_idx, pos, group_size[j]) ] = phi_scale[j] * inv_sqrt_scale_factor[j] * phi_tilde[ segment(group_idx, pos, group_size[j]) ];
      pos += group_size[j];
    }
    return phi;
  }


// Horseshoe Prior code from: https://discourse.mc-stan.org/t/user-friendly-implementation-of-regularised-horseshoe/8920/5
  /* Efficient computation of the horseshoe prior
   * see Appendix C.1 in https://projecteuclid.org/euclid.ejs/1513306866
   * Args:
   *   z: standardized population-level coefficients
   *   lambda: local shrinkage parameters
   *   tau: global shrinkage parameter
   *   c2: slap regularization parameter
   * Returns:
   *   population-level coefficients following the horseshoe prior
   */
vector horseshoe(vector z, vector lambda, real tau, real c2) {
    int K = rows(z);
    vector[K] lambda2 = square(lambda);
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return z .* lambda_tilde * tau;
  }
}

