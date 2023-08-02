#version 460
#extension GL_EXT_ray_tracing : require
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_scalar_block_layout : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : require
#extension GL_EXT_buffer_reference2 : require

#include "raycommon.glsl"
#include "wavefront.glsl"
#include "gltf.glsl"



layout(location = 0) rayPayloadInEXT hitPayload prd;
layout(location = 1) rayPayloadEXT bool isShadowed;


layout(set = 0, binding = eTlas ) uniform accelerationStructureEXT topLevelAS;
layout(set = 0, binding = ePrimLookup) readonly buffer _InstanceInfo {PrimMeshInfo primInfo[];};

layout(buffer_reference, scalar) readonly buffer Vertices  { vec3 v[]; }; // Positions of an object
layout(buffer_reference, scalar) readonly buffer Indices   { uint i[]; }; // Triangle indices
layout(buffer_reference, scalar) readonly buffer Normals   { vec3 n[]; }; // Triangle normals
layout(buffer_reference, scalar) readonly buffer TexCoords { vec2 t[]; }; // Texture Coordinates
layout(buffer_reference, scalar) readonly buffer Materials { GltfShadeMaterial m[]; };  // Material ID for each triangle

layout(set = 1, binding = eSceneDesc ) readonly buffer SceneDesc_ { SceneDesc sceneDesc; };
layout(set = 1, binding = eTextures) uniform sampler2D texturesMap[]; // all textures

layout(push_constant) uniform _PushConstantRay { PushConstantRay pcRay; };

hitAttributeEXT vec3 attribs;

void main()
{
    // Retrieve the Primitive mesh buffer information
    PrimMeshInfo pinfo = primInfo[gl_InstanceCustomIndexEXT];

    // Getting the 'first index' for this mesh (offset of the mesh + offset of the triangle)
    uint indexOffset  = pinfo.indexOffset + (3 * gl_PrimitiveID);
    uint vertexOffset = pinfo.vertexOffset;           // Vertex offset as defined in glTF
    uint matIndex     = max(0, pinfo.materialIndex);  // material of primitive mesh

    Materials gltfMat   = Materials(sceneDesc.materialAddress);
    Vertices  vertices  = Vertices(sceneDesc.vertexAddress);
    Indices   indices   = Indices(sceneDesc.indexAddress);
    Normals   normals   = Normals(sceneDesc.normalAddress);
    TexCoords texCoords = TexCoords(sceneDesc.uvAddress);
    Materials materials = Materials(sceneDesc.materialAddress);
  
    // Getting the 3 indices of the triangle (local)
    ivec3 triangleIndex = ivec3(indices.i[indexOffset + 0], indices.i[indexOffset + 1], indices.i[indexOffset + 2]);
    triangleIndex += ivec3(vertexOffset);  // (global)


    // Vertex of the triangle
    const vec3 pos0           = vertices.v[triangleIndex.x];
    const vec3 pos1           = vertices.v[triangleIndex.y];
    const vec3 pos2           = vertices.v[triangleIndex.z];
    
    const vec3 barycentrics = vec3(1.0 - attribs.x - attribs.y, attribs.x, attribs.y);

    // Computing the coordinates of the hit position
    const vec3 position      = pos0 * barycentrics.x + pos1 * barycentrics.y + pos2 * barycentrics.z;
    const vec3 world_position = vec3(gl_ObjectToWorldEXT * vec4(position, 1.0));  // Transforming the position to world space

    // Computing the normal at hit position
    const vec3 nrm0         = normals.n[triangleIndex.x];
    const vec3 nrm1         = normals.n[triangleIndex.y];
    const vec3 nrm2         = normals.n[triangleIndex.z];
    const vec3 nrm          = normalize(nrm0 * barycentrics.x + nrm1 * barycentrics.y + nrm2 * barycentrics.z);
    const vec3 world_normal = normalize(vec3(nrm * gl_WorldToObjectEXT));
    const vec3 geom_normal  = normalize(cross(pos1 - pos0, pos2 - pos0));
    
    // TexCoord
    const vec2 uv0       = texCoords.t[triangleIndex.x];
    const vec2 uv1       = texCoords.t[triangleIndex.y];
    const vec2 uv2       = texCoords.t[triangleIndex.z];
    const vec2 texcoord0 = uv0 * barycentrics.x + uv1 * barycentrics.y + uv2 * barycentrics.z;

    // Vector toward the light
    vec3  L;
    float lightIntensity = pcRay.lightIntensity;
    float lightDistance  = 100000.0;
    
    // Point light
    if(pcRay.lightType == 0)
    {
        vec3 lDir      = pcRay.lightPosition - world_position;
        lightDistance  = length(lDir);
        lightIntensity = pcRay.lightIntensity / (lightDistance * lightDistance);
        L              = normalize(lDir);
    }
    else  // Directional light
    {
        L = normalize(pcRay.lightPosition);
    }

    // Material of the object
    GltfShadeMaterial mat = materials.m[matIndex];

    // Diffuse
    vec3 diffuse = computeDiffuse(mat, L, world_normal);
    if(mat.pbrBaseColorTexture > -1)
    {
        uint txtId = mat.pbrBaseColorTexture;
        diffuse *= texture(texturesMap[nonuniformEXT(txtId)], texcoord0).xyz;
    }

    // Specular
    vec3  specular    = vec3(0);
    float attenuation = 1;

    // Tracing shadow ray only if the light is visible from the surface
    if(dot(world_normal, L) > 0)
    {
        float tMin   = 0.001;
        float tMax   = lightDistance;
        vec3  origin = gl_WorldRayOriginEXT + gl_WorldRayDirectionEXT * gl_HitTEXT;
        vec3  rayDir = L;
        uint  flags = gl_RayFlagsTerminateOnFirstHitEXT | gl_RayFlagsOpaqueEXT | gl_RayFlagsSkipClosestHitShaderEXT;
        isShadowed = true;
        traceRayEXT(topLevelAS,  // acceleration structure
            flags,       // rayFlags
            0xFF,        // cullMask
            0,           // sbtRecordOffset
            0,           // sbtRecordStride
            1,           // missIndex
            origin,      // ray origin
            tMin,        // ray min range
            rayDir,      // ray direction
            tMax,        // ray max range
            1            // payload (location = 1)
        );

        if(isShadowed)
        {
          attenuation = 0.3;
        }
        else
        {
            // Specular
            specular = computeSpecular(mat, gl_WorldRayDirectionEXT, L, world_normal);
        }
    }

  // Reflection
  // At the end of the closest hit shader, before setting prd.hitValue, we need to shoot a ray if the material is reflective.
  if(mat.illum == 3 && prd.depth < 10)
  {
    vec3 origin   = worldPos;
    vec3 rayDir   = reflect(gl_WorldRayDirectionEXT, normal);
    prd.attenuation *= mat.specular;

    prd.depth++;
    traceRayEXT(topLevelAS,         // acceleration structure
            gl_RayFlagsNoneEXT,  // rayFlags
            0xFF,               // cullMask
            0,                  // sbtRecordOffset
            0,                  // sbtRecordStride
            0,                  // missIndex
            origin,             // ray origin
            0.1,                // ray min range
            rayDir,             // ray direction
            100000.0,           // ray max range
            0                   // payload (location = 0)
    );
    prd.depth--;

    //The calculated hitValue needs to be accumulated, since the payload is global for the entire execution from raygen
    prd.hitValue += vec3(lightIntensity * attenuation * (diffuse + specular));
}
