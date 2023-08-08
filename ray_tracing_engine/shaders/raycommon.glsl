struct hitPayload
{
  vec3 hitValue;
  // to track the depth and the attenuation of the ray
  int  depth;
  vec3 attenuation;
  // to start new rays if wanted.
  int  done;
  vec3 rayOrigin;
  vec3 rayDir;
};
