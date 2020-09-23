function B = add_noise(A,r)
% add noise adds 0-or-1 noise to a given image A with intensity r.
% Every pixel will be replaced by a noise with probability r. Each noisy
% pixel has 1/2 probability being black or white.
[m,n] = size(A);
random_numbers1 = rand(m,n);
random_numbers2 = rand(m,n);
isNoise = random_numbers1 < r;
isBlack = random_numbers2 < 0.5;
B = A;
B(isNoise) = 1;
B(isNoise & isBlack) = 0;
