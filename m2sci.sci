function [ out1, out2 ] = m2sci_eig( in1, in2 )
  [ nout, nin ] = argn()

  if nin == 1
    if nout == 2
      [ out1, out2 ] = spec( in1 )
    else
      [ out1 ] = spec( in1 )
    end
  elseif nin == 2
    if nout == 2
      [ alpha, beta ] = spec( in1, in2 );
    else
      [ alpha, beta, Z ] = spec( in1, in2 );
    end
    zero_spots = beta == 0;
    beta( zero_spots ) = 1;
    alpha( zero_spots ) = %inf;
    out1 = alpha ./ beta;
    [ alpha, beta, Z ] = spec( in1, in2 );
    zero_spots = beta == 0;
    beta( zero_spots ) = 1;
    alpha( zero_spots ) = %inf;
    eigenvalues = alpha ./ beta;
    if nout == 2
      out1 = Z
      out2 = diag( eigenvalues )
    else
      out1 = eigenvalues
    end
  else
    error( 'm2sci_eig: case not implemented' )
  end
endfunction

function [ output, indices ] = m2sci_sort( in )
  [ output, indices ] = sort( in );
  output = output($:-1:1);
endfunction
