struct hitPayload
{
  vec3 hitValue;
  // to track the depth and the attenuation of the ray
  int  depth;
  vec3 attenuation;
};
