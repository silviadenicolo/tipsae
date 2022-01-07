if(spatial_err == 0 && temporal_err == 0) { // no spatial/temporal error: reffs equal to v
  reffs = v;
  for(i in 1:M_oos){
      reffs_oos[i] = 0;
    }
}else{
  if(spatial_err==1 && temporal_err==0) { // spatial error
    for(i in 1:D){// definition of s - reordering wrt the domains
      s[i] = sigma_s[1] * s_raw[indices_spat[i]] / scales_ICAR[indices_spat[i]];
    }
    for(i in 1:M_is){// reffs is the sum of unstructured and spatial errors
      reffs[i] = v[i] + s[indices_is[i]];
    }
    for(i in 1:M_oos){// only spatial error for oos areas
      reffs_oos[i] = s[indices_oos[i]];
    }
  }
  if(spatial_err==0 && temporal_err==1){//temporal error
    t = sigma_t[1] * t_raw / scale_factor_RW1; // definition of t
    for(i in 1:M_is) { // sum of unstr.and temporal errors
      reffs[i]=v[indices_temp[indices_is[i], 1]] +
        t[indices_temp[indices_is[i], 1], indices_temp[indices_is[i], 2]];
    }
    for(i in 1:M_oos) { // out of sample: related to the kind of missingness
      if(cat_ios[i] == 1) { // no data: reff=0
        reffs_oos[i] = 0;
      }
      if(cat_ios[i] == 2) { // previous temporal observation missing: no temporal effect
      reffs_oos[i] = v[indices_temp[indices_oos[i], 1]];
      }
      if(cat_ios[i] == 3) { // both temporal and area effect
        reffs_oos[i] = v[indices_temp[indices_oos[i], 1]] +
          t[indices_temp[indices_oos[i], 1], indices_temp[indices_oos[i], 2]];
      }
    }
  }
  if(spatial_err==1 && temporal_err==1){
    t = sigma_t[1] * t_raw / scale_factor_RW1; // definition of t
    for(i in 1:D){// definition of s - reordering wrt the domains
      s[i] = sigma_s[1] * s_raw[indices_spat[i]] / scales_ICAR[indices_spat[i]];
    }
    for(i in 1:M_is) { // sum of spatial and temporal errors
      reffs[i]=s[indices_temp[indices_is[i], 1]] +
        t[indices_temp[indices_is[i], 1], indices_temp[indices_is[i], 2]];
    }
    for(i in 1:M_oos) { // out of sample: related to the kind of missingness
      if(cat_ios[i] == 1) { // no data: reff=0
        reffs_oos[i] = s[indices_temp[indices_oos[i], 1]] +
        t[indices_temp[indices_oos[i], 1], indices_temp[indices_oos[i], 2]];
      }
      if(cat_ios[i] == 2) { // previous temporal observation missing: no temporal effect
        reffs_oos[i] = s[indices_temp[indices_oos[i], 1]] +
        t[indices_temp[indices_oos[i], 1], indices_temp[indices_oos[i], 2]];
      }
      if(cat_ios[i] == 3) { // both temporal and area effect
        reffs_oos[i] = s[indices_temp[indices_oos[i], 1]] +
          t[indices_temp[indices_oos[i], 1], indices_temp[indices_oos[i], 2]];
      }
    }
  }


}
