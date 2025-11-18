import{g as tn,r as nn,a as rn,b as on,c as mt,d as an,f as sn,e as ln,h as cn,i as hn,S as fn,j as un,k as De,l as Nt,C as dn,m as Dt,M as mn,n as Je,o as pn,p as Mt,A as Rt,q as vn,s as _n,t as gn,O as xn}from"./vendor-echarts-Dwmdrxdr.js";import{bC as F,bD as Q,bE as te,m as pt,b as Ae,bF as yn,bG as Ot,bH as L,bI as bn,bJ as Tn,bK as Pe,bL as Ve,bM as It,bN as wn,bO as U,bP as H,bQ as Sn,bR as An,bS as Cn,bT as Ln,bU as Pn,bV as En,bW as Nn,bX as Fe,bY as et,bZ as Dn,b_ as Mn,b$ as Rn,c0 as On,c1 as In,c2 as $,c3 as Fn,c4 as zn,c5 as Bn,c6 as Hn,af as Un,c7 as xe,c8 as Gn,c9 as kn,ca as Vn,aZ as Ft,cb as jn,B as zt,cc as Bt,cd as Xn,g as Ce,h as Wn,Z as Zn,a5 as Me,c as Yn,ce as je,ak as qn,w as Ee,a1 as ae,cf as ie,cg as se,ch as Kn,ci as Qn,cj as $n,ck as Jn,cl as ei,cm as ti,cn as ni,co as ii,cp as ri,cq as oi,cr as ai,cs as si,ct as li,ah as ci,cu as hi,$ as tt,k as Ht,a2 as fi,a6 as ui}from"./vendor-main-DSi3UDf9.js";var nt=["mousedown","mouseup","mousemove","mouseover","mouseout","click","dblclick","contextmenu"];function it(e){return"_on"+e}var rt=function(e){var t=this;this._texture=new F({anisotropic:32,flipY:!1,surface:this,dispose:function(n){t.dispose(),F.prototype.dispose.call(this,n)}}),nt.forEach(function(n){this[it(n)]=function(i){i.triangle&&this._meshes.forEach(function(r){this.dispatchEvent(n,r,i.triangle,i.point)},this)}},this),this._meshes=[],e&&this.setECharts(e),this.onupdate=null};rt.prototype={constructor:rt,getTexture:function(){return this._texture},setECharts:function(e){this._chart=e;var t=e.getDom();if(!(t instanceof HTMLCanvasElement))console.error("ECharts must init on canvas if it is used as texture."),t=document.createElement("canvas");else{var n=this,i=e.getZr(),r=i.__oldRefreshImmediately||i.refreshImmediately;i.refreshImmediately=function(){r.call(this),n._texture.dirty(),n.onupdate&&n.onupdate()},i.__oldRefreshImmediately=r}this._texture.image=t,this._texture.dirty(),this.onupdate&&this.onupdate()},dispatchEvent:(function(){var e=new Q,t=new Q,n=new Q,i=new te,r=new te,o=new te,a=new te,s=new Q;return function(c,l,h,f){var d=l.geometry,u=d.attributes.position,m=d.attributes.texcoord0,v=Q.dot,_=Q.cross;u.get(h[0],e.array),u.get(h[1],t.array),u.get(h[2],n.array),m.get(h[0],i.array),m.get(h[1],r.array),m.get(h[2],o.array),_(s,t,n);var g=v(e,s),x=v(f,s)/g;_(s,n,e);var b=v(f,s)/g;_(s,e,t);var T=v(f,s)/g;te.scale(a,i,x),te.scaleAndAdd(a,a,r,b),te.scaleAndAdd(a,a,o,T);var A=a.x*this._chart.getWidth(),P=a.y*this._chart.getHeight();this._chart.getZr().handler.dispatch(c,{zrX:A,zrY:P})}})(),attachToMesh:function(e){this._meshes.indexOf(e)>=0||(nt.forEach(function(t){e.on(t,this[it(t)],this)},this),this._meshes.push(e))},detachFromMesh:function(e){var t=this._meshes.indexOf(e);t>=0&&this._meshes.splice(t,1),nt.forEach(function(n){e.off(n,this[it(n)])},this)},dispose:function(){this._meshes.forEach(function(e){this.detachFromMesh(e)},this)}};var J={firstNotNull:function(){for(var e=0,t=arguments.length;e<t;e++)if(arguments[e]!=null)return arguments[e]},queryDataIndex:function(e,t){if(t.dataIndexInside!=null)return t.dataIndexInside;if(t.dataIndex!=null)return Ae(t.dataIndex)?pt(t.dataIndex,function(n){return e.indexOfRawIndex(n)}):e.indexOfRawIndex(t.dataIndex);if(t.name!=null)return Ae(t.name)?pt(t.name,function(n){return e.indexOfName(n)}):e.indexOfName(t.name)}},di={_animators:null,getAnimators:function(){return this._animators=this._animators||[],this._animators},animate:function(e,t){this._animators=this._animators||[];var n=this,i;if(e){for(var r=e.split("."),o=n,a=0,s=r.length;a<s;a++)o&&(o=o[r[a]]);o&&(i=o)}else i=n;if(i==null)throw new Error("Target "+e+" not exists");var c=this._animators,l=new yn(i,t),h=this;return l.during(function(){h.__zr&&h.__zr.refresh()}).done(function(){var f=c.indexOf(l);f>=0&&c.splice(f,1)}),c.push(l),this.__zr&&this.__zr.animation.addAnimator(l),l},stopAnimation:function(e){this._animators=this._animators||[];for(var t=this._animators,n=t.length,i=0;i<n;i++)t[i].stop(e);return t.length=0,this},addAnimatorsToZr:function(e){if(this._animators)for(var t=0;t<this._animators.length;t++)e.animation.addAnimator(this._animators[t])},removeAnimatorsFromZr:function(e){if(this._animators)for(var t=0;t<this._animators.length;t++)e.animation.removeAnimator(this._animators[t])}};const mi=`
@export ecgl.common.transformUniforms
uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;
uniform mat4 worldInverseTranspose : WORLDINVERSETRANSPOSE;
uniform mat4 world : WORLD;
@end

@export ecgl.common.attributes
attribute vec3 position : POSITION;
attribute vec2 texcoord : TEXCOORD_0;
attribute vec3 normal : NORMAL;
@end

@export ecgl.common.uv.header
uniform vec2 uvRepeat : [1.0, 1.0];
uniform vec2 uvOffset : [0.0, 0.0];
uniform vec2 detailUvRepeat : [1.0, 1.0];
uniform vec2 detailUvOffset : [0.0, 0.0];

varying vec2 v_Texcoord;
varying vec2 v_DetailTexcoord;
@end

@export ecgl.common.uv.main
v_Texcoord = texcoord * uvRepeat + uvOffset;
v_DetailTexcoord = texcoord * detailUvRepeat + detailUvOffset;
@end

@export ecgl.common.uv.fragmentHeader
varying vec2 v_Texcoord;
varying vec2 v_DetailTexcoord;
@end


@export ecgl.common.albedo.main

 vec4 albedoTexel = vec4(1.0);
#ifdef DIFFUSEMAP_ENABLED
 albedoTexel = texture2D(diffuseMap, v_Texcoord);
 #ifdef SRGB_DECODE
 albedoTexel = sRGBToLinear(albedoTexel);
 #endif
#endif

#ifdef DETAILMAP_ENABLED
 vec4 detailTexel = texture2D(detailMap, v_DetailTexcoord);
 #ifdef SRGB_DECODE
 detailTexel = sRGBToLinear(detailTexel);
 #endif
 albedoTexel.rgb = mix(albedoTexel.rgb, detailTexel.rgb, detailTexel.a);
 albedoTexel.a = detailTexel.a + (1.0 - detailTexel.a) * albedoTexel.a;
#endif

@end

@export ecgl.common.wireframe.vertexHeader

#ifdef WIREFRAME_QUAD
attribute vec4 barycentric;
varying vec4 v_Barycentric;
#elif defined(WIREFRAME_TRIANGLE)
attribute vec3 barycentric;
varying vec3 v_Barycentric;
#endif

@end

@export ecgl.common.wireframe.vertexMain

#if defined(WIREFRAME_QUAD) || defined(WIREFRAME_TRIANGLE)
 v_Barycentric = barycentric;
#endif

@end


@export ecgl.common.wireframe.fragmentHeader

uniform float wireframeLineWidth : 1;
uniform vec4 wireframeLineColor: [0, 0, 0, 0.5];

#ifdef WIREFRAME_QUAD
varying vec4 v_Barycentric;
float edgeFactor () {
 vec4 d = fwidth(v_Barycentric);
 vec4 a4 = smoothstep(vec4(0.0), d * wireframeLineWidth, v_Barycentric);
 return min(min(min(a4.x, a4.y), a4.z), a4.w);
}
#elif defined(WIREFRAME_TRIANGLE)
varying vec3 v_Barycentric;
float edgeFactor () {
 vec3 d = fwidth(v_Barycentric);
 vec3 a3 = smoothstep(vec3(0.0), d * wireframeLineWidth, v_Barycentric);
 return min(min(a3.x, a3.y), a3.z);
}
#endif

@end


@export ecgl.common.wireframe.fragmentMain

#if defined(WIREFRAME_QUAD) || defined(WIREFRAME_TRIANGLE)
 if (wireframeLineWidth > 0.) {
 vec4 lineColor = wireframeLineColor;
#ifdef SRGB_DECODE
 lineColor = sRGBToLinear(lineColor);
#endif

 gl_FragColor.rgb = mix(gl_FragColor.rgb, lineColor.rgb, (1.0 - edgeFactor()) * lineColor.a);
 }
#endif
@end




@export ecgl.common.bumpMap.header

#ifdef BUMPMAP_ENABLED
uniform sampler2D bumpMap;
uniform float bumpScale : 1.0;


vec3 bumpNormal(vec3 surfPos, vec3 surfNormal, vec3 baseNormal)
{
 vec2 dSTdx = dFdx(v_Texcoord);
 vec2 dSTdy = dFdy(v_Texcoord);

 float Hll = bumpScale * texture2D(bumpMap, v_Texcoord).x;
 float dHx = bumpScale * texture2D(bumpMap, v_Texcoord + dSTdx).x - Hll;
 float dHy = bumpScale * texture2D(bumpMap, v_Texcoord + dSTdy).x - Hll;

 vec3 vSigmaX = dFdx(surfPos);
 vec3 vSigmaY = dFdy(surfPos);
 vec3 vN = surfNormal;

 vec3 R1 = cross(vSigmaY, vN);
 vec3 R2 = cross(vN, vSigmaX);

 float fDet = dot(vSigmaX, R1);

 vec3 vGrad = sign(fDet) * (dHx * R1 + dHy * R2);
 return normalize(abs(fDet) * baseNormal - vGrad);

}
#endif

@end

@export ecgl.common.normalMap.vertexHeader

#ifdef NORMALMAP_ENABLED
attribute vec4 tangent : TANGENT;
varying vec3 v_Tangent;
varying vec3 v_Bitangent;
#endif

@end

@export ecgl.common.normalMap.vertexMain

#ifdef NORMALMAP_ENABLED
 if (dot(tangent, tangent) > 0.0) {
 v_Tangent = normalize((worldInverseTranspose * vec4(tangent.xyz, 0.0)).xyz);
 v_Bitangent = normalize(cross(v_Normal, v_Tangent) * tangent.w);
 }
#endif

@end


@export ecgl.common.normalMap.fragmentHeader

#ifdef NORMALMAP_ENABLED
uniform sampler2D normalMap;
varying vec3 v_Tangent;
varying vec3 v_Bitangent;
#endif

@end

@export ecgl.common.normalMap.fragmentMain
#ifdef NORMALMAP_ENABLED
 if (dot(v_Tangent, v_Tangent) > 0.0) {
 vec3 normalTexel = texture2D(normalMap, v_DetailTexcoord).xyz;
 if (dot(normalTexel, normalTexel) > 0.0) { N = normalTexel * 2.0 - 1.0;
 mat3 tbn = mat3(v_Tangent, v_Bitangent, v_Normal);
 N = normalize(tbn * N);
 }
 }
#endif
@end



@export ecgl.common.vertexAnimation.header

#ifdef VERTEX_ANIMATION
attribute vec3 prevPosition;
attribute vec3 prevNormal;
uniform float percent;
#endif

@end

@export ecgl.common.vertexAnimation.main

#ifdef VERTEX_ANIMATION
 vec3 pos = mix(prevPosition, position, percent);
 vec3 norm = mix(prevNormal, normal, percent);
#else
 vec3 pos = position;
 vec3 norm = normal;
#endif

@end


@export ecgl.common.ssaoMap.header
#ifdef SSAOMAP_ENABLED
uniform sampler2D ssaoMap;
uniform vec4 viewport : VIEWPORT;
#endif
@end

@export ecgl.common.ssaoMap.main
 float ao = 1.0;
#ifdef SSAOMAP_ENABLED
 ao = texture2D(ssaoMap, (gl_FragCoord.xy - viewport.xy) / viewport.zw).r;
#endif
@end




@export ecgl.common.diffuseLayer.header

#if (LAYER_DIFFUSEMAP_COUNT > 0)
uniform float layerDiffuseIntensity[LAYER_DIFFUSEMAP_COUNT];
uniform sampler2D layerDiffuseMap[LAYER_DIFFUSEMAP_COUNT];
#endif

@end

@export ecgl.common.emissiveLayer.header

#if (LAYER_EMISSIVEMAP_COUNT > 0)
uniform float layerEmissionIntensity[LAYER_EMISSIVEMAP_COUNT];
uniform sampler2D layerEmissiveMap[LAYER_EMISSIVEMAP_COUNT];
#endif

@end

@export ecgl.common.layers.header
@import ecgl.common.diffuseLayer.header
@import ecgl.common.emissiveLayer.header
@end

@export ecgl.common.diffuseLayer.main

#if (LAYER_DIFFUSEMAP_COUNT > 0)
 for (int _idx_ = 0; _idx_ < LAYER_DIFFUSEMAP_COUNT; _idx_++) {{
 float intensity = layerDiffuseIntensity[_idx_];
 vec4 texel2 = texture2D(layerDiffuseMap[_idx_], v_Texcoord);
 #ifdef SRGB_DECODE
 texel2 = sRGBToLinear(texel2);
 #endif
 albedoTexel.rgb = mix(albedoTexel.rgb, texel2.rgb * intensity, texel2.a);
 albedoTexel.a = texel2.a + (1.0 - texel2.a) * albedoTexel.a;
 }}
#endif

@end

@export ecgl.common.emissiveLayer.main

#if (LAYER_EMISSIVEMAP_COUNT > 0)
 for (int _idx_ = 0; _idx_ < LAYER_EMISSIVEMAP_COUNT; _idx_++)
 {{
 vec4 texel2 = texture2D(layerEmissiveMap[_idx_], v_Texcoord) * layerEmissionIntensity[_idx_];
 #ifdef SRGB_DECODE
 texel2 = sRGBToLinear(texel2);
 #endif
 float intensity = layerEmissionIntensity[_idx_];
 gl_FragColor.rgb += texel2.rgb * texel2.a * intensity;
 }}
#endif

@end
`,pi=`@export ecgl.color.vertex

uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;

@import ecgl.common.uv.header

attribute vec2 texcoord : TEXCOORD_0;
attribute vec3 position: POSITION;

@import ecgl.common.wireframe.vertexHeader

#ifdef VERTEX_COLOR
attribute vec4 a_Color : COLOR;
varying vec4 v_Color;
#endif

#ifdef VERTEX_ANIMATION
attribute vec3 prevPosition;
uniform float percent : 1.0;
#endif

#ifdef ATMOSPHERE_ENABLED
attribute vec3 normal: NORMAL;
uniform mat4 worldInverseTranspose : WORLDINVERSETRANSPOSE;
varying vec3 v_Normal;
#endif

void main()
{
#ifdef VERTEX_ANIMATION
 vec3 pos = mix(prevPosition, position, percent);
#else
 vec3 pos = position;
#endif

 gl_Position = worldViewProjection * vec4(pos, 1.0);

 @import ecgl.common.uv.main

#ifdef VERTEX_COLOR
 v_Color = a_Color;
#endif

#ifdef ATMOSPHERE_ENABLED
 v_Normal = normalize((worldInverseTranspose * vec4(normal, 0.0)).xyz);
#endif

 @import ecgl.common.wireframe.vertexMain

}

@end

@export ecgl.color.fragment

#define LAYER_DIFFUSEMAP_COUNT 0
#define LAYER_EMISSIVEMAP_COUNT 0

uniform sampler2D diffuseMap;
uniform sampler2D detailMap;

uniform vec4 color : [1.0, 1.0, 1.0, 1.0];

#ifdef ATMOSPHERE_ENABLED
uniform mat4 viewTranspose: VIEWTRANSPOSE;
uniform vec3 glowColor;
uniform float glowPower;
varying vec3 v_Normal;
#endif

#ifdef VERTEX_COLOR
varying vec4 v_Color;
#endif

@import ecgl.common.layers.header

@import ecgl.common.uv.fragmentHeader

@import ecgl.common.wireframe.fragmentHeader

@import clay.util.srgb

void main()
{
#ifdef SRGB_DECODE
 gl_FragColor = sRGBToLinear(color);
#else
 gl_FragColor = color;
#endif

#ifdef VERTEX_COLOR
 gl_FragColor *= v_Color;
#endif

 @import ecgl.common.albedo.main

 @import ecgl.common.diffuseLayer.main

 gl_FragColor *= albedoTexel;

#ifdef ATMOSPHERE_ENABLED
 float atmoIntensity = pow(1.0 - dot(v_Normal, (viewTranspose * vec4(0.0, 0.0, 1.0, 0.0)).xyz), glowPower);
 gl_FragColor.rgb += glowColor * atmoIntensity;
#endif

 @import ecgl.common.emissiveLayer.main

 @import ecgl.common.wireframe.fragmentMain

}
@end`,vi=`/**
 * http: */

@export ecgl.lambert.vertex

@import ecgl.common.transformUniforms

@import ecgl.common.uv.header


@import ecgl.common.attributes

@import ecgl.common.wireframe.vertexHeader

#ifdef VERTEX_COLOR
attribute vec4 a_Color : COLOR;
varying vec4 v_Color;
#endif


@import ecgl.common.vertexAnimation.header


varying vec3 v_Normal;
varying vec3 v_WorldPosition;

void main()
{
 @import ecgl.common.uv.main

 @import ecgl.common.vertexAnimation.main


 gl_Position = worldViewProjection * vec4(pos, 1.0);

 v_Normal = normalize((worldInverseTranspose * vec4(norm, 0.0)).xyz);
 v_WorldPosition = (world * vec4(pos, 1.0)).xyz;

#ifdef VERTEX_COLOR
 v_Color = a_Color;
#endif

 @import ecgl.common.wireframe.vertexMain
}

@end


@export ecgl.lambert.fragment

#define LAYER_DIFFUSEMAP_COUNT 0
#define LAYER_EMISSIVEMAP_COUNT 0

#define NORMAL_UP_AXIS 1
#define NORMAL_FRONT_AXIS 2

@import ecgl.common.uv.fragmentHeader

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

uniform sampler2D diffuseMap;
uniform sampler2D detailMap;

@import ecgl.common.layers.header

uniform float emissionIntensity: 1.0;

uniform vec4 color : [1.0, 1.0, 1.0, 1.0];

uniform mat4 viewInverse : VIEWINVERSE;

#ifdef ATMOSPHERE_ENABLED
uniform mat4 viewTranspose: VIEWTRANSPOSE;
uniform vec3 glowColor;
uniform float glowPower;
#endif

#ifdef AMBIENT_LIGHT_COUNT
@import clay.header.ambient_light
#endif
#ifdef AMBIENT_SH_LIGHT_COUNT
@import clay.header.ambient_sh_light
#endif

#ifdef DIRECTIONAL_LIGHT_COUNT
@import clay.header.directional_light
#endif

#ifdef VERTEX_COLOR
varying vec4 v_Color;
#endif


@import ecgl.common.ssaoMap.header

@import ecgl.common.bumpMap.header

@import clay.util.srgb

@import ecgl.common.wireframe.fragmentHeader

@import clay.plugin.compute_shadow_map

void main()
{
#ifdef SRGB_DECODE
 gl_FragColor = sRGBToLinear(color);
#else
 gl_FragColor = color;
#endif

#ifdef VERTEX_COLOR
 #ifdef SRGB_DECODE
 gl_FragColor *= sRGBToLinear(v_Color);
 #else
 gl_FragColor *= v_Color;
 #endif
#endif

 @import ecgl.common.albedo.main

 @import ecgl.common.diffuseLayer.main

 gl_FragColor *= albedoTexel;

 vec3 N = v_Normal;
#ifdef DOUBLE_SIDED
 vec3 eyePos = viewInverse[3].xyz;
 vec3 V = normalize(eyePos - v_WorldPosition);

 if (dot(N, V) < 0.0) {
 N = -N;
 }
#endif

 float ambientFactor = 1.0;

#ifdef BUMPMAP_ENABLED
 N = bumpNormal(v_WorldPosition, v_Normal, N);
 ambientFactor = dot(v_Normal, N);
#endif

 vec3 N2 = vec3(N.x, N[NORMAL_UP_AXIS], N[NORMAL_FRONT_AXIS]);

 vec3 diffuseColor = vec3(0.0, 0.0, 0.0);

 @import ecgl.common.ssaoMap.main

#ifdef AMBIENT_LIGHT_COUNT
 for(int i = 0; i < AMBIENT_LIGHT_COUNT; i++)
 {
 diffuseColor += ambientLightColor[i] * ambientFactor * ao;
 }
#endif
#ifdef AMBIENT_SH_LIGHT_COUNT
 for(int _idx_ = 0; _idx_ < AMBIENT_SH_LIGHT_COUNT; _idx_++)
 {{
 diffuseColor += calcAmbientSHLight(_idx_, N2) * ambientSHLightColor[_idx_] * ao;
 }}
#endif
#ifdef DIRECTIONAL_LIGHT_COUNT
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 float shadowContribsDir[DIRECTIONAL_LIGHT_COUNT];
 if(shadowEnabled)
 {
 computeShadowOfDirectionalLights(v_WorldPosition, shadowContribsDir);
 }
#endif
 for(int i = 0; i < DIRECTIONAL_LIGHT_COUNT; i++)
 {
 vec3 lightDirection = -directionalLightDirection[i];
 vec3 lightColor = directionalLightColor[i];

 float shadowContrib = 1.0;
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 if (shadowEnabled)
 {
 shadowContrib = shadowContribsDir[i];
 }
#endif

 float ndl = dot(N, normalize(lightDirection)) * shadowContrib;

 diffuseColor += lightColor * clamp(ndl, 0.0, 1.0);
 }
#endif

 gl_FragColor.rgb *= diffuseColor;

#ifdef ATMOSPHERE_ENABLED
 float atmoIntensity = pow(1.0 - dot(v_Normal, (viewTranspose * vec4(0.0, 0.0, 1.0, 0.0)).xyz), glowPower);
 gl_FragColor.rgb += glowColor * atmoIntensity;
#endif

 @import ecgl.common.emissiveLayer.main

 @import ecgl.common.wireframe.fragmentMain
}

@end`,_i=`@export ecgl.realistic.vertex

@import ecgl.common.transformUniforms

@import ecgl.common.uv.header

@import ecgl.common.attributes


@import ecgl.common.wireframe.vertexHeader

#ifdef VERTEX_COLOR
attribute vec4 a_Color : COLOR;
varying vec4 v_Color;
#endif

#ifdef NORMALMAP_ENABLED
attribute vec4 tangent : TANGENT;
varying vec3 v_Tangent;
varying vec3 v_Bitangent;
#endif

@import ecgl.common.vertexAnimation.header

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

void main()
{

 @import ecgl.common.uv.main

 @import ecgl.common.vertexAnimation.main

 gl_Position = worldViewProjection * vec4(pos, 1.0);

 v_Normal = normalize((worldInverseTranspose * vec4(norm, 0.0)).xyz);
 v_WorldPosition = (world * vec4(pos, 1.0)).xyz;

#ifdef VERTEX_COLOR
 v_Color = a_Color;
#endif

#ifdef NORMALMAP_ENABLED
 v_Tangent = normalize((worldInverseTranspose * vec4(tangent.xyz, 0.0)).xyz);
 v_Bitangent = normalize(cross(v_Normal, v_Tangent) * tangent.w);
#endif

 @import ecgl.common.wireframe.vertexMain

}

@end



@export ecgl.realistic.fragment

#define LAYER_DIFFUSEMAP_COUNT 0
#define LAYER_EMISSIVEMAP_COUNT 0
#define PI 3.14159265358979
#define ROUGHNESS_CHANEL 0
#define METALNESS_CHANEL 1

#define NORMAL_UP_AXIS 1
#define NORMAL_FRONT_AXIS 2

#ifdef VERTEX_COLOR
varying vec4 v_Color;
#endif

@import ecgl.common.uv.fragmentHeader

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

uniform sampler2D diffuseMap;

uniform sampler2D detailMap;
uniform sampler2D metalnessMap;
uniform sampler2D roughnessMap;

@import ecgl.common.layers.header

uniform float emissionIntensity: 1.0;

uniform vec4 color : [1.0, 1.0, 1.0, 1.0];

uniform float metalness : 0.0;
uniform float roughness : 0.5;

uniform mat4 viewInverse : VIEWINVERSE;

#ifdef ATMOSPHERE_ENABLED
uniform mat4 viewTranspose: VIEWTRANSPOSE;
uniform vec3 glowColor;
uniform float glowPower;
#endif

#ifdef AMBIENT_LIGHT_COUNT
@import clay.header.ambient_light
#endif

#ifdef AMBIENT_SH_LIGHT_COUNT
@import clay.header.ambient_sh_light
#endif

#ifdef AMBIENT_CUBEMAP_LIGHT_COUNT
@import clay.header.ambient_cubemap_light
#endif

#ifdef DIRECTIONAL_LIGHT_COUNT
@import clay.header.directional_light
#endif

@import ecgl.common.normalMap.fragmentHeader

@import ecgl.common.ssaoMap.header

@import ecgl.common.bumpMap.header

@import clay.util.srgb

@import clay.util.rgbm

@import ecgl.common.wireframe.fragmentHeader

@import clay.plugin.compute_shadow_map

vec3 F_Schlick(float ndv, vec3 spec) {
 return spec + (1.0 - spec) * pow(1.0 - ndv, 5.0);
}

float D_Phong(float g, float ndh) {
 float a = pow(8192.0, g);
 return (a + 2.0) / 8.0 * pow(ndh, a);
}

void main()
{
 vec4 albedoColor = color;

 vec3 eyePos = viewInverse[3].xyz;
 vec3 V = normalize(eyePos - v_WorldPosition);
#ifdef VERTEX_COLOR
 #ifdef SRGB_DECODE
 albedoColor *= sRGBToLinear(v_Color);
 #else
 albedoColor *= v_Color;
 #endif
#endif

 @import ecgl.common.albedo.main

 @import ecgl.common.diffuseLayer.main

 albedoColor *= albedoTexel;

 float m = metalness;

#ifdef METALNESSMAP_ENABLED
 float m2 = texture2D(metalnessMap, v_DetailTexcoord)[METALNESS_CHANEL];
 m = clamp(m2 + (m - 0.5) * 2.0, 0.0, 1.0);
#endif

 vec3 baseColor = albedoColor.rgb;
 albedoColor.rgb = baseColor * (1.0 - m);
 vec3 specFactor = mix(vec3(0.04), baseColor, m);

 float g = 1.0 - roughness;

#ifdef ROUGHNESSMAP_ENABLED
 float g2 = 1.0 - texture2D(roughnessMap, v_DetailTexcoord)[ROUGHNESS_CHANEL];
 g = clamp(g2 + (g - 0.5) * 2.0, 0.0, 1.0);
#endif

 vec3 N = v_Normal;

#ifdef DOUBLE_SIDED
 if (dot(N, V) < 0.0) {
 N = -N;
 }
#endif

 float ambientFactor = 1.0;

#ifdef BUMPMAP_ENABLED
 N = bumpNormal(v_WorldPosition, v_Normal, N);
 ambientFactor = dot(v_Normal, N);
#endif

@import ecgl.common.normalMap.fragmentMain

 vec3 N2 = vec3(N.x, N[NORMAL_UP_AXIS], N[NORMAL_FRONT_AXIS]);

 vec3 diffuseTerm = vec3(0.0);
 vec3 specularTerm = vec3(0.0);

 float ndv = clamp(dot(N, V), 0.0, 1.0);
 vec3 fresnelTerm = F_Schlick(ndv, specFactor);

 @import ecgl.common.ssaoMap.main

#ifdef AMBIENT_LIGHT_COUNT
 for(int _idx_ = 0; _idx_ < AMBIENT_LIGHT_COUNT; _idx_++)
 {{
 diffuseTerm += ambientLightColor[_idx_] * ambientFactor * ao;
 }}
#endif

#ifdef AMBIENT_SH_LIGHT_COUNT
 for(int _idx_ = 0; _idx_ < AMBIENT_SH_LIGHT_COUNT; _idx_++)
 {{
 diffuseTerm += calcAmbientSHLight(_idx_, N2) * ambientSHLightColor[_idx_] * ao;
 }}
#endif

#ifdef DIRECTIONAL_LIGHT_COUNT
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 float shadowContribsDir[DIRECTIONAL_LIGHT_COUNT];
 if(shadowEnabled)
 {
 computeShadowOfDirectionalLights(v_WorldPosition, shadowContribsDir);
 }
#endif
 for(int _idx_ = 0; _idx_ < DIRECTIONAL_LIGHT_COUNT; _idx_++)
 {{
 vec3 L = -directionalLightDirection[_idx_];
 vec3 lc = directionalLightColor[_idx_];

 vec3 H = normalize(L + V);
 float ndl = clamp(dot(N, normalize(L)), 0.0, 1.0);
 float ndh = clamp(dot(N, H), 0.0, 1.0);

 float shadowContrib = 1.0;
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 if (shadowEnabled)
 {
 shadowContrib = shadowContribsDir[_idx_];
 }
#endif

 vec3 li = lc * ndl * shadowContrib;

 diffuseTerm += li;
 specularTerm += li * fresnelTerm * D_Phong(g, ndh);
 }}
#endif


#ifdef AMBIENT_CUBEMAP_LIGHT_COUNT
 vec3 L = reflect(-V, N);
 L = vec3(L.x, L[NORMAL_UP_AXIS], L[NORMAL_FRONT_AXIS]);
 float rough2 = clamp(1.0 - g, 0.0, 1.0);
 float bias2 = rough2 * 5.0;
 vec2 brdfParam2 = texture2D(ambientCubemapLightBRDFLookup[0], vec2(rough2, ndv)).xy;
 vec3 envWeight2 = specFactor * brdfParam2.x + brdfParam2.y;
 vec3 envTexel2;
 for(int _idx_ = 0; _idx_ < AMBIENT_CUBEMAP_LIGHT_COUNT; _idx_++)
 {{
 envTexel2 = RGBMDecode(textureCubeLodEXT(ambientCubemapLightCubemap[_idx_], L, bias2), 8.12);
 specularTerm += ambientCubemapLightColor[_idx_] * envTexel2 * envWeight2 * ao;
 }}
#endif

 gl_FragColor.rgb = albedoColor.rgb * diffuseTerm + specularTerm;
 gl_FragColor.a = albedoColor.a;

#ifdef ATMOSPHERE_ENABLED
 float atmoIntensity = pow(1.0 - dot(v_Normal, (viewTranspose * vec4(0.0, 0.0, 1.0, 0.0)).xyz), glowPower);
 gl_FragColor.rgb += glowColor * atmoIntensity;
#endif

#ifdef SRGB_ENCODE
 gl_FragColor = linearTosRGB(gl_FragColor);
#endif

 @import ecgl.common.emissiveLayer.main

 @import ecgl.common.wireframe.fragmentMain
}

@end`,gi=`@export ecgl.hatching.vertex

@import ecgl.realistic.vertex

@end


@export ecgl.hatching.fragment

#define NORMAL_UP_AXIS 1
#define NORMAL_FRONT_AXIS 2

@import ecgl.common.uv.fragmentHeader

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

uniform vec4 color : [0.0, 0.0, 0.0, 1.0];
uniform vec4 paperColor : [1.0, 1.0, 1.0, 1.0];

uniform mat4 viewInverse : VIEWINVERSE;

#ifdef AMBIENT_LIGHT_COUNT
@import clay.header.ambient_light
#endif
#ifdef AMBIENT_SH_LIGHT_COUNT
@import clay.header.ambient_sh_light
#endif

#ifdef DIRECTIONAL_LIGHT_COUNT
@import clay.header.directional_light
#endif

#ifdef VERTEX_COLOR
varying vec4 v_Color;
#endif


@import ecgl.common.ssaoMap.header

@import ecgl.common.bumpMap.header

@import clay.util.srgb

@import ecgl.common.wireframe.fragmentHeader

@import clay.plugin.compute_shadow_map

uniform sampler2D hatch1;
uniform sampler2D hatch2;
uniform sampler2D hatch3;
uniform sampler2D hatch4;
uniform sampler2D hatch5;
uniform sampler2D hatch6;

float shade(in float tone) {
 vec4 c = vec4(1. ,1., 1., 1.);
 float step = 1. / 6.;
 vec2 uv = v_DetailTexcoord;
 if (tone <= step / 2.0) {
 c = mix(vec4(0.), texture2D(hatch6, uv), 12. * tone);
 }
 else if (tone <= step) {
 c = mix(texture2D(hatch6, uv), texture2D(hatch5, uv), 6. * tone);
 }
 if(tone > step && tone <= 2. * step){
 c = mix(texture2D(hatch5, uv), texture2D(hatch4, uv) , 6. * (tone - step));
 }
 if(tone > 2. * step && tone <= 3. * step){
 c = mix(texture2D(hatch4, uv), texture2D(hatch3, uv), 6. * (tone - 2. * step));
 }
 if(tone > 3. * step && tone <= 4. * step){
 c = mix(texture2D(hatch3, uv), texture2D(hatch2, uv), 6. * (tone - 3. * step));
 }
 if(tone > 4. * step && tone <= 5. * step){
 c = mix(texture2D(hatch2, uv), texture2D(hatch1, uv), 6. * (tone - 4. * step));
 }
 if(tone > 5. * step){
 c = mix(texture2D(hatch1, uv), vec4(1.), 6. * (tone - 5. * step));
 }

 return c.r;
}

const vec3 w = vec3(0.2125, 0.7154, 0.0721);

void main()
{
#ifdef SRGB_DECODE
 vec4 inkColor = sRGBToLinear(color);
#else
 vec4 inkColor = color;
#endif

#ifdef VERTEX_COLOR
 #ifdef SRGB_DECODE
 inkColor *= sRGBToLinear(v_Color);
 #else
 inkColor *= v_Color;
 #endif
#endif

 vec3 N = v_Normal;
#ifdef DOUBLE_SIDED
 vec3 eyePos = viewInverse[3].xyz;
 vec3 V = normalize(eyePos - v_WorldPosition);

 if (dot(N, V) < 0.0) {
 N = -N;
 }
#endif

 float tone = 0.0;

 float ambientFactor = 1.0;

#ifdef BUMPMAP_ENABLED
 N = bumpNormal(v_WorldPosition, v_Normal, N);
 ambientFactor = dot(v_Normal, N);
#endif

 vec3 N2 = vec3(N.x, N[NORMAL_UP_AXIS], N[NORMAL_FRONT_AXIS]);

 @import ecgl.common.ssaoMap.main

#ifdef AMBIENT_LIGHT_COUNT
 for(int i = 0; i < AMBIENT_LIGHT_COUNT; i++)
 {
 tone += dot(ambientLightColor[i], w) * ambientFactor * ao;
 }
#endif
#ifdef AMBIENT_SH_LIGHT_COUNT
 for(int _idx_ = 0; _idx_ < AMBIENT_SH_LIGHT_COUNT; _idx_++)
 {{
 tone += dot(calcAmbientSHLight(_idx_, N2) * ambientSHLightColor[_idx_], w) * ao;
 }}
#endif
#ifdef DIRECTIONAL_LIGHT_COUNT
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 float shadowContribsDir[DIRECTIONAL_LIGHT_COUNT];
 if(shadowEnabled)
 {
 computeShadowOfDirectionalLights(v_WorldPosition, shadowContribsDir);
 }
#endif
 for(int i = 0; i < DIRECTIONAL_LIGHT_COUNT; i++)
 {
 vec3 lightDirection = -directionalLightDirection[i];
 float lightTone = dot(directionalLightColor[i], w);

 float shadowContrib = 1.0;
#if defined(DIRECTIONAL_LIGHT_SHADOWMAP_COUNT)
 if (shadowEnabled)
 {
 shadowContrib = shadowContribsDir[i];
 }
#endif

 float ndl = dot(N, normalize(lightDirection)) * shadowContrib;

 tone += lightTone * clamp(ndl, 0.0, 1.0);
 }
#endif

 gl_FragColor = mix(inkColor, paperColor, shade(clamp(tone, 0.0, 1.0)));
 }
@end
`,xi=`@export ecgl.sm.depth.vertex

uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;

attribute vec3 position : POSITION;
attribute vec2 texcoord : TEXCOORD_0;

#ifdef VERTEX_ANIMATION
attribute vec3 prevPosition;
uniform float percent : 1.0;
#endif

varying vec4 v_ViewPosition;
varying vec2 v_Texcoord;

void main(){

#ifdef VERTEX_ANIMATION
 vec3 pos = mix(prevPosition, position, percent);
#else
 vec3 pos = position;
#endif

 v_ViewPosition = worldViewProjection * vec4(pos, 1.0);
 gl_Position = v_ViewPosition;

 v_Texcoord = texcoord;

}
@end



@export ecgl.sm.depth.fragment

@import clay.sm.depth.fragment

@end`;Object.assign(Ot.prototype,di);L.import(bn);L.import(Tn);L.import(mi);L.import(pi);L.import(vi);L.import(_i);L.import(gi);L.import(xi);function yi(e){return!e||e==="none"}function Ut(e){return e instanceof HTMLCanvasElement||e instanceof HTMLImageElement||e instanceof Image}function bi(e){return e.getZr&&e.setOption}var Ti=Pe.prototype.addToScene,wi=Pe.prototype.removeFromScene;Pe.prototype.addToScene=function(e){if(Ti.call(this,e),this.__zr){var t=this.__zr;e.traverse(function(n){n.__zr=t,n.addAnimatorsToZr&&n.addAnimatorsToZr(t)})}};Pe.prototype.removeFromScene=function(e){wi.call(this,e),e.traverse(function(t){var n=t.__zr;t.__zr=null,n&&t.removeAnimatorsFromZr&&t.removeAnimatorsFromZr(n)})};Ve.prototype.setTextureImage=function(e,t,n,i){if(this.shader){var r=n.getZr(),o=this,a;return o.autoUpdateTextureStatus=!1,o.disableTexture(e),yi(t)||(a=p.loadTexture(t,n,i,function(s){o.enableTexture(e),r&&r.refresh()}),o.set(e,a)),a}};var p={};p.Renderer=It;p.Node=Ot;p.Mesh=wn;p.Shader=L;p.Material=Ve;p.Texture=U;p.Texture2D=F;p.Geometry=H;p.SphereGeometry=Sn;p.PlaneGeometry=An;p.CubeGeometry=Cn;p.AmbientLight=Ln;p.DirectionalLight=Pn;p.PointLight=En;p.SpotLight=Nn;p.PerspectiveCamera=Fe;p.OrthographicCamera=et;p.Vector2=te;p.Vector3=Q;p.Vector4=Dn;p.Quaternion=Mn;p.Matrix2=Rn;p.Matrix2d=On;p.Matrix3=In;p.Matrix4=$;p.Plane=Fn;p.Ray=zn;p.BoundingBox=Bn;p.Frustum=Hn;var Re=null;function Si(){return Re!==null||(Re=xe.createBlank("rgba(255,255,255,0)").image),Re}function vt(e){return Math.pow(2,Math.round(Math.log(e)/Math.LN2))}function _t(e){if((e.wrapS===U.REPEAT||e.wrapT===U.REPEAT)&&e.image){var t=vt(e.width),n=vt(e.height);if(t!==e.width||n!==e.height){var i=document.createElement("canvas");i.width=t,i.height=n;var r=i.getContext("2d");r.drawImage(e.image,0,0,t,n),e.image=i}}}p.loadTexture=function(e,t,n,i){typeof n=="function"&&(i=n,n={}),n=n||{};for(var r=Object.keys(n).sort(),o="",a=0;a<r.length;a++)o+=r[a]+"_"+n[r[a]]+"_";var s=t.__textureCache=t.__textureCache||new Un(20);if(bi(e)){var c=e.__textureid__,l=s.get(o+c);if(l)l.texture.surface.setECharts(e),i&&i(l.texture);else{var h=new rt(e);h.onupdate=function(){t.getZr().refresh()},l={texture:h.getTexture()};for(var a=0;a<r.length;a++)l.texture[r[a]]=n[r[a]];c=e.__textureid__||"__ecgl_ec__"+l.texture.__uid__,e.__textureid__=c,s.put(o+c,l),i&&i(l.texture)}return l.texture}else if(Ut(e)){var c=e.__textureid__,l=s.get(o+c);if(!l){l={texture:new p.Texture2D({image:e})};for(var a=0;a<r.length;a++)l.texture[r[a]]=n[r[a]];c=e.__textureid__||"__ecgl_image__"+l.texture.__uid__,e.__textureid__=c,s.put(o+c,l),_t(l.texture),i&&i(l.texture)}return l.texture}else{var l=s.get(o+e);if(l)l.callbacks?l.callbacks.push(i):i&&i(l.texture);else if(e.match(/.hdr$|^data:application\/octet-stream/)){l={callbacks:[i]};var f=xe.loadTexture(e,{exposure:n.exposure,fileType:"hdr"},function(){f.dirty(),l.callbacks.forEach(function(m){m&&m(f)}),l.callbacks=null});l.texture=f,s.put(o+e,l)}else{for(var f=new p.Texture2D({image:new Image}),a=0;a<r.length;a++)f[r[a]]=n[r[a]];l={texture:f,callbacks:[i]};var d=f.image;d.onload=function(){f.image=d,_t(f),f.dirty(),l.callbacks.forEach(function(v){v&&v(f)}),l.callbacks=null},d.crossOrigin="Anonymous",d.src=e,f.image=Si(),s.put(o+e,l)}return l.texture}};p.createAmbientCubemap=function(e,t,n,i){e=e||{};var r=e.texture,o=J.firstNotNull(e.exposure,1),a=new Gn({intensity:J.firstNotNull(e.specularIntensity,1)}),s=new kn({intensity:J.firstNotNull(e.diffuseIntensity,1),coefficients:[.844,.712,.691,-.037,.083,.167,.343,.288,.299,-.041,-.021,-.009,-.003,-.041,-.064,-.011,-.007,-.004,-.031,.034,.081,-.06,-.049,-.06,.046,.056,.05]});return a.cubemap=p.loadTexture(r,n,{exposure:o},function(){a.cubemap.flipY=!1,a.prefilter(t,32),s.coefficients=Vn.projectEnvironmentMap(t,a.cubemap,{lod:1}),i&&i()}),{specular:a,diffuse:s}};p.createBlankTexture=xe.createBlank;p.isImage=Ut;p.additiveBlend=function(e){e.blendEquation(e.FUNC_ADD),e.blendFunc(e.SRC_ALPHA,e.ONE)};p.parseColor=function(e,t){return e instanceof Array?(t||(t=[]),t[0]=e[0],t[1]=e[1],t[2]=e[2],e.length>3?t[3]=e[3]:t[3]=1,t):(t=Ft(e||"#000",t)||[0,0,0,0],t[0]/=255,t[1]/=255,t[2]/=255,t)};p.directionFromAlphaBeta=function(e,t){var n=e/180*Math.PI+Math.PI/2,i=-t/180*Math.PI+Math.PI/2,r=[],o=Math.sin(n);return r[0]=o*Math.cos(i),r[1]=-Math.cos(n),r[2]=o*Math.sin(i),r};p.getShadowResolution=function(e){var t=1024;switch(e){case"low":t=512;break;case"medium":break;case"high":t=2048;break;case"ultra":t=4096;break}return t};p.COMMON_SHADERS=["lambert","color","realistic","hatching","shadow"];p.createShader=function(e){e==="ecgl.shadow"&&(e="ecgl.displayShadow");var t=L.source(e+".vertex"),n=L.source(e+".fragment");t||console.error("Vertex shader of '%s' not exits",e),n||console.error("Fragment shader of '%s' not exits",e);var i=new L(t,n);return i.name=e,i};p.createMaterial=function(e,t){t instanceof Array||(t=[t]);var n=p.createShader(e),i=new Ve({shader:n});return t.forEach(function(r){typeof r=="string"&&i.define(r)}),i};p.setMaterialFromModel=function(e,t,n,i){t.autoUpdateTextureStatus=!1;var r=n.getModel(e+"Material"),o=r.get("detailTexture"),a=J.firstNotNull(r.get("textureTiling"),1),s=J.firstNotNull(r.get("textureOffset"),0);typeof a=="number"&&(a=[a,a]),typeof s=="number"&&(s=[s,s]);var c=a[0]>1||a[1]>1?p.Texture.REPEAT:p.Texture.CLAMP_TO_EDGE,l={anisotropic:8,wrapS:c,wrapT:c};if(e==="realistic"){var h=r.get("roughness"),f=r.get("metalness");f!=null?isNaN(f)&&(t.setTextureImage("metalnessMap",f,i,l),f=J.firstNotNull(r.get("metalnessAdjust"),.5)):f=0,h!=null?isNaN(h)&&(t.setTextureImage("roughnessMap",h,i,l),h=J.firstNotNull(r.get("roughnessAdjust"),.5)):h=.5;var d=r.get("normalTexture");t.setTextureImage("detailMap",o,i,l),t.setTextureImage("normalMap",d,i,l),t.set({roughness:h,metalness:f,detailUvRepeat:a,detailUvOffset:s})}else if(e==="lambert")t.setTextureImage("detailMap",o,i,l),t.set({detailUvRepeat:a,detailUvOffset:s});else if(e==="color")t.setTextureImage("detailMap",o,i,l),t.set({detailUvRepeat:a,detailUvOffset:s});else if(e==="hatching"){var u=r.get("hatchingTextures")||[];u.length<6;for(var m=0;m<6;m++)t.setTextureImage("hatch"+(m+1),u[m],i,{anisotropic:8,wrapS:p.Texture.REPEAT,wrapT:p.Texture.REPEAT});t.set({detailUvRepeat:a,detailUvOffset:s})}};p.updateVertexAnimation=function(e,t,n,i){var r=i.get("animation"),o=i.get("animationDurationUpdate"),a=i.get("animationEasingUpdate"),s=n.shadowDepthMaterial;if(r&&t&&o>0&&t.geometry.vertexCount===n.geometry.vertexCount){n.material.define("vertex","VERTEX_ANIMATION"),n.ignorePreZ=!0,s&&s.define("vertex","VERTEX_ANIMATION");for(var c=0;c<e.length;c++)n.geometry.attributes[e[c][0]].value=t.geometry.attributes[e[c][1]].value;n.geometry.dirty(),n.__percent=0,n.material.set("percent",0),n.stopAnimation(),n.animate().when(o,{__percent:1}).during(function(){n.material.set("percent",n.__percent),s&&s.set("percent",n.__percent)}).done(function(){n.ignorePreZ=!1,n.material.undefine("vertex","VERTEX_ANIMATION"),s&&s.undefine("vertex","VERTEX_ANIMATION")}).start(a)}else n.material.undefine("vertex","VERTEX_ANIMATION"),s&&s.undefine("vertex","VERTEX_ANIMATION")};var E=function(e,t){this.id=e,this.zr=t;try{this.renderer=new It({clearBit:0,devicePixelRatio:t.painter.dpr,preserveDrawingBuffer:!0,premultipliedAlpha:!0}),this.renderer.resize(t.painter.getWidth(),t.painter.getHeight())}catch(i){this.renderer=null,this.dom=document.createElement("div"),this.dom.style.cssText="position:absolute; left: 0; top: 0; right: 0; bottom: 0;",this.dom.className="ecgl-nowebgl",this.dom.innerHTML="Sorry, your browser does not support WebGL",console.error(i);return}this.onglobalout=this.onglobalout.bind(this),t.on("globalout",this.onglobalout),this.dom=this.renderer.canvas;var n=this.dom.style;n.position="absolute",n.left="0",n.top="0",this.views=[],this._picking=new jn({renderer:this.renderer}),this._viewsToDispose=[],this._accumulatingId=0,this._zrEventProxy=new zt({shape:{x:-1,y:-1,width:2,height:2},__isGLToZRProxy:!0}),this._backgroundColor=null,this._disposed=!1};E.prototype.setUnpainted=function(){};E.prototype.addView=function(e){if(e.layer!==this){var t=this._viewsToDispose.indexOf(e);t>=0&&this._viewsToDispose.splice(t,1),this.views.push(e),e.layer=this;var n=this.zr;e.scene.traverse(function(i){i.__zr=n,i.addAnimatorsToZr&&i.addAnimatorsToZr(n)})}};function Gt(e){var t=e.__zr;e.__zr=null,t&&e.removeAnimatorsFromZr&&e.removeAnimatorsFromZr(t)}E.prototype.removeView=function(e){if(e.layer===this){var t=this.views.indexOf(e);t>=0&&(this.views.splice(t,1),e.scene.traverse(Gt,this),e.layer=null,this._viewsToDispose.push(e))}};E.prototype.removeViewsAll=function(){this.views.forEach(function(e){e.scene.traverse(Gt,this),e.layer=null,this._viewsToDispose.push(e)},this),this.views.length=0};E.prototype.resize=function(e,t){var n=this.renderer;n.resize(e,t)};E.prototype.clear=function(){var e=this.renderer.gl,t=this._backgroundColor||[0,0,0,0];e.clearColor(t[0],t[1],t[2],t[3]),e.depthMask(!0),e.colorMask(!0,!0,!0,!0),e.clear(e.DEPTH_BUFFER_BIT|e.COLOR_BUFFER_BIT)};E.prototype.clearDepth=function(){var e=this.renderer.gl;e.clear(e.DEPTH_BUFFER_BIT)};E.prototype.clearColor=function(){var e=this.renderer.gl;e.clearColor(0,0,0,0),e.clear(e.COLOR_BUFFER_BIT)};E.prototype.needsRefresh=function(){this.zr.refresh()};E.prototype.refresh=function(e){this._backgroundColor=e?p.parseColor(e):[0,0,0,0],this.renderer.clearColor=this._backgroundColor;for(var t=0;t<this.views.length;t++)this.views[t].prepareRender(this.renderer);this._doRender(!1),this._trackAndClean();for(var t=0;t<this._viewsToDispose.length;t++)this._viewsToDispose[t].dispose(this.renderer);this._viewsToDispose.length=0,this._startAccumulating()};E.prototype.renderToCanvas=function(e){this._startAccumulating(!0),e.drawImage(this.dom,0,0,e.canvas.width,e.canvas.height)};E.prototype._doRender=function(e){this.clear(),this.renderer.saveViewport();for(var t=0;t<this.views.length;t++)this.views[t].render(this.renderer,e);this.renderer.restoreViewport()};E.prototype._stopAccumulating=function(){this._accumulatingId=0,clearTimeout(this._accumulatingTimeout)};var Ai=1;E.prototype._startAccumulating=function(e){var t=this;this._stopAccumulating();for(var n=!1,i=0;i<this.views.length;i++)n=this.views[i].needsAccumulate()||n;if(!n)return;function r(o){if(!(!t._accumulatingId||o!==t._accumulatingId)){for(var a=!0,s=0;s<t.views.length;s++)a=t.views[s].isAccumulateFinished()&&n;a||(t._doRender(!0),e?r(o):Xn(function(){r(o)}))}}this._accumulatingId=Ai++,e?r(t._accumulatingId):this._accumulatingTimeout=setTimeout(function(){r(t._accumulatingId)},50)};E.prototype._trackAndClean=function(){var e=[],t=[];this._textureList&&(ze(this._textureList),ze(this._geometriesList));for(var n=0;n<this.views.length;n++)Ci(this.views[n].scene,e,t);this._textureList&&(Be(this.renderer,this._textureList),Be(this.renderer,this._geometriesList)),this._textureList=e,this._geometriesList=t};function ze(e){for(var t=0;t<e.length;t++)e[t].__used__=0}function Be(e,t){for(var n=0;n<t.length;n++)t[n].__used__||t[n].dispose(e)}function Oe(e,t){e.__used__=e.__used__||0,e.__used__++,e.__used__===1&&t.push(e)}function Ci(e,t,n){var i,r;e.traverse(function(a){if(a.isRenderable()){var s=a.geometry,c=a.material;if(c!==i)for(var l=c.getTextureUniforms(),h=0;h<l.length;h++){var f=l[h],d=c.uniforms[f].value;if(d){if(d instanceof U)Oe(d,t);else if(d instanceof Array)for(var u=0;u<d.length;u++)d[u]instanceof U&&Oe(d[u],t)}}s!==r&&Oe(s,n),i=c,r=s}});for(var o=0;o<e.lights.length;o++)e.lights[o].cubemap&&Oe(e.lights[o].cubemap,t)}E.prototype.dispose=function(){this._disposed||(this._stopAccumulating(),this._textureList&&(ze(this._textureList),ze(this._geometriesList),Be(this.renderer,this._textureList),Be(this.renderer,this._geometriesList)),this.zr.off("globalout",this.onglobalout),this._disposed=!0)};E.prototype.onmousedown=function(e){if(!(e.target&&e.target.__isGLToZRProxy)){e=e.event;var t=this.pickObject(e.offsetX,e.offsetY);t&&(this._dispatchEvent("mousedown",e,t),this._dispatchDataEvent("mousedown",e,t)),this._downX=e.offsetX,this._downY=e.offsetY}};E.prototype.onmousemove=function(e){if(!(e.target&&e.target.__isGLToZRProxy)){e=e.event;var t=this.pickObject(e.offsetX,e.offsetY),n=t&&t.target,i=this._hovered;this._hovered=t,i&&n!==i.target&&(i.relatedTarget=n,this._dispatchEvent("mouseout",e,i),this.zr.setCursorStyle("default")),this._dispatchEvent("mousemove",e,t),t&&(this.zr.setCursorStyle("pointer"),(!i||n!==i.target)&&this._dispatchEvent("mouseover",e,t)),this._dispatchDataEvent("mousemove",e,t)}};E.prototype.onmouseup=function(e){if(!(e.target&&e.target.__isGLToZRProxy)){e=e.event;var t=this.pickObject(e.offsetX,e.offsetY);t&&(this._dispatchEvent("mouseup",e,t),this._dispatchDataEvent("mouseup",e,t)),this._upX=e.offsetX,this._upY=e.offsetY}};E.prototype.onclick=E.prototype.dblclick=function(e){if(!(e.target&&e.target.__isGLToZRProxy)){var t=this._upX-this._downX,n=this._upY-this._downY;if(!(Math.sqrt(t*t+n*n)>20)){e=e.event;var i=this.pickObject(e.offsetX,e.offsetY);i&&(this._dispatchEvent(e.type,e,i),this._dispatchDataEvent(e.type,e,i));var r=this._clickToSetFocusPoint(e);if(r){var o=r.view.setDOFFocusOnPoint(r.distance);o&&this.zr.refresh()}}}};E.prototype._clickToSetFocusPoint=function(e){for(var t=this.renderer,n=t.viewport,i=this.views.length-1;i>=0;i--){var r=this.views[i];if(r.hasDOF()&&r.containPoint(e.offsetX,e.offsetY)){this._picking.scene=r.scene,this._picking.camera=r.camera,t.viewport=r.viewport;var o=this._picking.pick(e.offsetX,e.offsetY,!0);if(o)return o.view=r,o}}t.viewport=n};E.prototype.onglobalout=function(e){var t=this._hovered;t&&this._dispatchEvent("mouseout",e,{target:t.target})};E.prototype.pickObject=function(e,t){for(var n=[],i=this.renderer,r=i.viewport,o=0;o<this.views.length;o++){var a=this.views[o];a.containPoint(e,t)&&(this._picking.scene=a.scene,this._picking.camera=a.camera,i.viewport=a.viewport,this._picking.pickAll(e,t,n))}return i.viewport=r,n.sort(function(s,c){return s.distance-c.distance}),n[0]};E.prototype._dispatchEvent=function(e,t,n){n||(n={});var i=n.target;for(n.cancelBubble=!1,n.event=t,n.type=e,n.offsetX=t.offsetX,n.offsetY=t.offsetY;i&&(i.trigger(e,n),i=i.getParent(),!n.cancelBubble););this._dispatchToView(e,n)};E.prototype._dispatchDataEvent=function(e,t,n){var i=n&&n.target,r=i&&i.dataIndex,o=i&&i.seriesIndex,a=i&&i.eventData,s=!1,c=this._zrEventProxy;c.x=t.offsetX,c.y=t.offsetY,c.update();var l={target:c};const h=tn(c);e==="mousemove"&&(r!=null?r!==this._lastDataIndex&&(parseInt(this._lastDataIndex,10)>=0&&(h.dataIndex=this._lastDataIndex,h.seriesIndex=this._lastSeriesIndex,this.zr.handler.dispatchToElement(l,"mouseout",t)),s=!0):a!=null&&a!==this._lastEventData&&(this._lastEventData!=null&&(h.eventData=this._lastEventData,this.zr.handler.dispatchToElement(l,"mouseout",t)),s=!0),this._lastEventData=a,this._lastDataIndex=r,this._lastSeriesIndex=o),h.eventData=a,h.dataIndex=r,h.seriesIndex=o,(a!=null||parseInt(r,10)>=0&&parseInt(o,10)>=0)&&(this.zr.handler.dispatchToElement(l,e,t),s&&this.zr.handler.dispatchToElement(l,"mouseover",t))};E.prototype._dispatchToView=function(e,t){for(var n=0;n<this.views.length;n++)this.views[n].containPoint(t.offsetX,t.offsetY)&&this.views[n].trigger(e,t)};Object.assign(E.prototype,Bt);var Li=["bar3D","line3D","map3D","scatter3D","surface","lines3D","scatterGL","scatter3D"];function Se(e,t){if(e&&e[t]&&(e[t].normal||e[t].emphasis)){var n=e[t].normal,i=e[t].emphasis;n&&(e[t]=n),i&&(e.emphasis=e.emphasis||{},e.emphasis[t]=i)}}function Pi(e){Se(e,"itemStyle"),Se(e,"lineStyle"),Se(e,"areaStyle"),Se(e,"label")}function Ie(e){e&&(e instanceof Array||(e=[e]),Ce(e,function(t){if(t.axisLabel){var n=t.axisLabel;Object.assign(n,n.textStyle),n.textStyle=null}}))}function Ei(e){Ce(e.series,function(t){Wn(Li,t.type)>=0&&(Pi(t),t.coordinateSystem==="mapbox"&&(t.coordinateSystem="mapbox3D",e.mapbox3D=e.mapbox))}),Ie(e.xAxis3D),Ie(e.yAxis3D),Ie(e.zAxis3D),Ie(e.grid3D),Se(e.geo3D)}function kt(e){this._layers={},this._zr=e}kt.prototype.update=function(e,t){var n=this,i=t.getZr();if(!i.getWidth()||!i.getHeight()){console.warn("Dom has no width or height");return}function r(s){i.setSleepAfterStill(0);var c;s.coordinateSystem&&s.coordinateSystem.model,c=s.get("zlevel");var l=n._layers,h=l[c];if(!h){if(h=l[c]=new E("gl-"+c,i),i.painter.isSingleCanvas()){h.virtual=!0;var f=new Zn({z:1e4,style:{image:h.renderer.canvas},silent:!0});h.__hostImage=f,i.add(f)}i.painter.insertLayer(c,h)}return h.__hostImage&&h.__hostImage.setStyle({width:h.renderer.getWidth(),height:h.renderer.getHeight()}),h}function o(s,c){s&&s.traverse(function(l){l.isRenderable&&l.isRenderable()&&(l.ignorePicking=l.$ignorePicking!=null?l.$ignorePicking:c)})}for(var a in this._layers)this._layers[a].removeViewsAll();e.eachComponent(function(s,c){if(s!=="series"){var l=t.getViewOfComponentModel(c),h=c.coordinateSystem;if(l.__ecgl__){var f;if(h){if(!h.viewGL){console.error("Can't find viewGL in coordinateSystem of component "+c.id);return}f=h.viewGL}else{if(!c.viewGL){console.error("Can't find viewGL of component "+c.id);return}f=h.viewGL}var f=h.viewGL,d=r(c);d.addView(f),l.afterRender&&l.afterRender(c,e,t,d),o(l.groupGL,c.get("silent"))}}}),e.eachSeries(function(s){var c=t.getViewOfSeriesModel(s),l=s.coordinateSystem;if(c.__ecgl__){if(l&&!l.viewGL&&!c.viewGL){console.error("Can't find viewGL of series "+c.id);return}var h=l&&l.viewGL||c.viewGL,f=r(s);f.addView(h),c.afterRender&&c.afterRender(s,e,t,f),o(c.groupGL,s.get("silent"))}})};nn(function(e){var t=e.getZr(),n=t.painter.dispose;t.painter.dispose=function(){typeof this.eachOtherLayer=="function"&&this.eachOtherLayer(function(i){i instanceof E&&i.dispose()}),n.call(this)},t.painter.getRenderedCanvas=function(i){if(i=i||{},this._singleCanvas)return this._layers[0].dom;var r=document.createElement("canvas"),o=i.pixelRatio||this.dpr;r.width=this.getWidth()*o,r.height=this.getHeight()*o;var a=r.getContext("2d");a.dpr=o,a.clearRect(0,0,r.width,r.height),i.backgroundColor&&(a.fillStyle=i.backgroundColor,a.fillRect(0,0,r.width,r.height));var s=this.storage.getDisplayList(!0),c={},l,h=this;function f(v,_){var g=h._zlevelList;v==null&&(v=-1/0);for(var x,b=0;b<g.length;b++){var T=g[b],A=h._layers[T];if(!A.__builtin__&&T>v&&T<_){x=A;break}}x&&x.renderToCanvas&&(a.save(),x.renderToCanvas(a),a.restore())}for(var d={ctx:a},u=0;u<s.length;u++){var m=s[u];m.zlevel!==l&&(f(l,m.zlevel),l=m.zlevel),this._doPaintEl(m,d,!0,null,c)}return f(l,1/0),r}});rn(function(e,t){var n=t.getZr(),i=n.__egl=n.__egl||new kt(n);i.update(e,t)});on(Ei);const Ni={defaultOption:{shading:null,realisticMaterial:{textureTiling:1,textureOffset:0,detailTexture:null},lambertMaterial:{textureTiling:1,textureOffset:0,detailTexture:null},colorMaterial:{textureTiling:1,textureOffset:0,detailTexture:null},hatchingMaterial:{textureTiling:1,textureOffset:0,paperColor:"#fff"}}};function ot(e,t){const n=e.getItemVisual(t,"style");if(n){const i=e.getVisual("drawType");return n[i]}}function gt(e,t){const n=e.getItemVisual(t,"style");return n&&n.opacity}function Di(e,t){var n=[];return Ce(e.dimensions,function(i){var r=e.getDimensionInfo(i),o=r.otherDims,a=o[t];a!=null&&a!==!1&&(n[a]=r.name)}),n}function Mi(e,t,n){function i(f){var d=[],u=Di(r,"tooltip");u.length?Ce(u,function(v){m(r.get(v,t),v)}):Ce(f,m);function m(v,_){var g=r.getDimensionInfo(_);if(!(!g||g.otherDims.tooltip===!1)){var x=g.type,b="- "+(g.tooltipName||g.name)+": "+(x==="ordinal"?v+"":x==="time"?sn("yyyy/MM/dd hh:mm:ss",v):mt(v));b&&d.push(Me(b))}}return"<br/>"+d.join("<br/>")}var r=e.getData(),o=e.getRawValue(t),a=Ae(o)?i(o):Me(mt(o)),s=r.getName(t),c=ot(r,t);Yn(c)&&c.colorStops&&(c=(c.colorStops[0]||{}).color),c=c||"transparent";var l=an(c),h=e.name;return h==="\0-"&&(h=""),h=h?Me(h)+"<br/>":"",h+l+(s?Me(s)+": "+a:a)}function Ri(e,t,n){n=n||e.getSource();var i=t||ln(e.get("coordinateSystem"))||["x","y","z"],r=cn(n,{dimensionsDefine:n.dimensionsDefine||e.get("dimensions"),encodeDefine:n.encodeDefine||e.get("encode"),coordDimensions:i.map(function(s){var c=e.getReferringComponents(s+"Axis3D").models[0];return{type:c&&c.get("type")==="category"?"ordinal":"float",name:s}})});e.get("coordinateSystem")==="cartesian3D"&&r.forEach(function(s){if(i.indexOf(s.coordDim)>=0){var c=e.getReferringComponents(s.coordDim+"Axis3D").models[0];c&&c.get("type")==="category"&&(s.ordinalMeta=c.getOrdinalMeta())}});var o=hn.enableDataStack(e,r,{byIndex:!0,stackedCoordDimension:"z"}),a=new fn(r,e);return a.setCalculationInfo(o),a.initData(n),a}const ht={convertToDynamicArray:function(e){e&&this.resetOffset();var t=this.attributes;for(var n in t)e||!t[n].value?t[n].value=[]:t[n].value=Array.prototype.slice.call(t[n].value);e||!this.indices?this.indices=[]:this.indices=Array.prototype.slice.call(this.indices)},convertToTypedArray:function(){var e=this.attributes;for(var t in e)e[t].value&&e[t].value.length>0?e[t].value=new Float32Array(e[t].value):e[t].value=null;this.indices&&this.indices.length>0&&(this.indices=this.vertexCount>65535?new Uint32Array(this.indices):new Uint16Array(this.indices)),this.dirty()}};function qe(e,t,n){var i=e[t];e[t]=e[n],e[n]=i}function Vt(e,t,n,i,r){var o=n,a=e[t];qe(e,t,i);for(var s=n;s<i;s++)r(e[s],a)<0&&(qe(e,s,o),o++);return qe(e,i,o),o}function He(e,t,n,i){if(n<i){var r=Math.floor((n+i)/2),o=Vt(e,r,n,i,t);He(e,t,n,o-1),He(e,t,o+1,i)}}function Ue(){this._parts=[]}Ue.prototype.step=function(e,t,n){var i=e.length;if(n===0){this._parts=[],this._sorted=!1;var r=Math.floor(i/2);this._parts.push({pivot:r,left:0,right:i-1}),this._currentSortPartIdx=0}if(!this._sorted){var o=this._parts;if(o.length===0)return this._sorted=!0,!0;if(o.length<512){for(var a=0;a<o.length;a++)o[a].pivot=Vt(e,o[a].pivot,o[a].left,o[a].right,t);for(var s=[],a=0;a<o.length;a++){var c=o[a].left,l=o[a].pivot-1;l>c&&s.push({pivot:Math.floor((l+c)/2),left:c,right:l});var c=o[a].pivot+1,l=o[a].right;l>c&&s.push({pivot:Math.floor((l+c)/2),left:c,right:l})}o=this._parts=s}else for(var a=0;a<Math.floor(o.length/10);a++){var h=o.length-1-this._currentSortPartIdx;if(He(e,t,o[h].left,o[h].right),this._currentSortPartIdx++,this._currentSortPartIdx===o.length)return this._sorted=!0,!0}return!1}};Ue.sort=He;var ye=je.vec3,xt=ye.create(),yt=ye.create(),bt=ye.create();const Oi={needsSortTriangles:function(){return this.indices&&this.sortTriangles},needsSortTrianglesProgressively:function(){return this.needsSortTriangles()&&this.triangleCount>=2e4},doSortTriangles:function(e,t){var n=this.indices;if(t===0){var i=this.attributes.position,e=e.array;(!this._triangleZList||this._triangleZList.length!==this.triangleCount)&&(this._triangleZList=new Float32Array(this.triangleCount),this._sortedTriangleIndices=new Uint32Array(this.triangleCount),this._indicesTmp=new n.constructor(n.length),this._triangleZListTmp=new Float32Array(this.triangleCount));for(var r=0,o,a=0;a<n.length;){i.get(n[a++],xt),i.get(n[a++],yt),i.get(n[a++],bt);var s=ye.sqrDist(xt,e),c=ye.sqrDist(yt,e),l=ye.sqrDist(bt,e),h=Math.min(s,c);h=Math.min(h,l),a===3?(o=h,h=0):h=h-o,this._triangleZList[r++]=h}}for(var f=this._sortedTriangleIndices,a=0;a<f.length;a++)f[a]=a;if(this.triangleCount<2e4)t===0&&this._simpleSort(!0);else for(var a=0;a<3;a++)this._progressiveQuickSort(t*3+a);for(var d=this._indicesTmp,u=this._triangleZListTmp,m=this._triangleZList,a=0;a<this.triangleCount;a++){var v=f[a]*3,_=a*3;d[_++]=n[v++],d[_++]=n[v++],d[_]=n[v],u[a]=m[f[a]]}var g=this._indicesTmp;this._indicesTmp=this.indices,this.indices=g;var g=this._triangleZListTmp;this._triangleZListTmp=this._triangleZList,this._triangleZList=g,this.dirtyIndices()},_simpleSort:function(e){var t=this._triangleZList,n=this._sortedTriangleIndices;function i(r,o){return t[o]-t[r]}e?Array.prototype.sort.call(n,i):Ue.sort(n,i,0,n.length-1)},_progressiveQuickSort:function(e){var t=this._triangleZList,n=this._sortedTriangleIndices;this._quickSort=this._quickSort||new Ue,this._quickSort.step(n,function(i,r){return t[r]-t[i]},e)}};function Ge(e,t,n,i,r,o,a){this._zr=e,this._x=0,this._y=0,this._rowHeight=0,this.width=i,this.height=r,this.offsetX=t,this.offsetY=n,this.dpr=a,this.gap=o}Ge.prototype={constructor:Ge,clear:function(){this._x=0,this._y=0,this._rowHeight=0},add:function(e,t,n){var i=e.getBoundingRect();t==null&&(t=i.width),n==null&&(n=i.height),t*=this.dpr,n*=this.dpr,this._fitElement(e,t,n);var r=this._x,o=this._y,a=this.width*this.dpr,s=this.height*this.dpr,c=this.gap;if(r+t+c>a&&(r=this._x=0,o+=this._rowHeight+c,this._y=o,this._rowHeight=0),this._x+=t+c,this._rowHeight=Math.max(this._rowHeight,n),o+n+c>s)return null;e.x+=this.offsetX*this.dpr+r,e.y+=this.offsetY*this.dpr+o,this._zr.add(e);var l=[this.offsetX/this.width,this.offsetY/this.height],h=[[r/a+l[0],o/s+l[1]],[(r+t)/a+l[0],(o+n)/s+l[1]]];return h},_fitElement:function(e,t,n){var i=e.getBoundingRect(),r=t/i.width,o=n/i.height;e.x=-i.x*r,e.y=-i.y*o,e.scaleX=r,e.scaleY=o,e.update()}};function at(e){e=e||{},e.width=e.width||512,e.height=e.height||512,e.devicePixelRatio=e.devicePixelRatio||1,e.gap=e.gap==null?2:e.gap;var t=document.createElement("canvas");t.width=e.width*e.devicePixelRatio,t.height=e.height*e.devicePixelRatio,this._canvas=t,this._texture=new F({image:t,flipY:!1});var n=this;this._zr=qn(t);var i=this._zr.refreshImmediately;this._zr.refreshImmediately=function(){i.call(this),n._texture.dirty(),n.onupdate&&n.onupdate()},this._dpr=e.devicePixelRatio,this._coords={},this.onupdate=e.onupdate,this._gap=e.gap,this._textureAtlasNodes=[new Ge(this._zr,0,0,e.width,e.height,this._gap,this._dpr)],this._nodeWidth=e.width,this._nodeHeight=e.height,this._currentNodeIdx=0}at.prototype={clear:function(){for(var e=0;e<this._textureAtlasNodes.length;e++)this._textureAtlasNodes[e].clear();this._currentNodeIdx=0,this._zr.clear(),this._coords={}},getWidth:function(){return this._width},getHeight:function(){return this._height},getTexture:function(){return this._texture},getDevicePixelRatio:function(){return this._dpr},getZr:function(){return this._zr},_getCurrentNode:function(){return this._textureAtlasNodes[this._currentNodeIdx]},_expand:function(){if(this._currentNodeIdx++,this._textureAtlasNodes[this._currentNodeIdx])return this._textureAtlasNodes[this._currentNodeIdx];var e=4096/this._dpr,t=this._textureAtlasNodes,n=t.length,i=n*this._nodeWidth%e,r=Math.floor(n*this._nodeWidth/e)*this._nodeHeight;if(!(r>=e)){var o=(i+this._nodeWidth)*this._dpr,a=(r+this._nodeHeight)*this._dpr;try{this._zr.resize({width:o,height:a})}catch{this._canvas.width=o,this._canvas.height=a}var s=new Ge(this._zr,i,r,this._nodeWidth,this._nodeHeight,this._gap,this._dpr);return this._textureAtlasNodes.push(s),s}},add:function(e,t,n){if(this._coords[e.id])return this._coords[e.id];var i=this._getCurrentNode().add(e,t,n);if(!i){var r=this._expand();if(!r)return;i=r.add(e,t,n)}return this._coords[e.id]=i,i},getCoordsScale:function(){var e=this._dpr;return[this._nodeWidth/this._canvas.width*e,this._nodeHeight/this._canvas.height*e]},getCoords:function(e){return this._coords[e]},dispose:function(){this._zr.dispose()}};var Tt=[0,1,2,0,2,3],jt=H.extend(function(){return{attributes:{position:new H.Attribute("position","float",3,"POSITION"),texcoord:new H.Attribute("texcoord","float",2,"TEXCOORD_0"),offset:new H.Attribute("offset","float",2),color:new H.Attribute("color","float",4,"COLOR")}}},{resetOffset:function(){this._vertexOffset=0,this._faceOffset=0},setSpriteCount:function(e){this._spriteCount=e;var t=e*4,n=e*2;this.vertexCount!==t&&(this.attributes.position.init(t),this.attributes.offset.init(t),this.attributes.color.init(t)),this.triangleCount!==n&&(this.indices=t>65535?new Uint32Array(n*3):new Uint16Array(n*3))},setSpriteAlign:function(e,t,n,i,r){n==null&&(n="left"),i==null&&(i="top");var o,a,s,c;switch(r=r||0,n){case"left":o=r,s=t[0]+r;break;case"center":case"middle":o=-t[0]/2,s=t[0]/2;break;case"right":o=-t[0]-r,s=-r;break}switch(i){case"bottom":a=r,c=t[1]+r;break;case"middle":a=-t[1]/2,c=t[1]/2;break;case"top":a=-t[1]-r,c=-r;break}var l=e*4,h=this.attributes.offset;h.set(l,[o,c]),h.set(l+1,[s,c]),h.set(l+2,[s,a]),h.set(l+3,[o,a])},addSprite:function(e,t,n,i,r,o){var a=this._vertexOffset;this.setSprite(this._vertexOffset/4,e,t,n,i,r,o);for(var s=0;s<Tt.length;s++)this.indices[this._faceOffset*3+s]=Tt[s]+a;return this._faceOffset+=2,this._vertexOffset+=4,a/4},setSprite:function(e,t,n,i,r,o,a){for(var s=e*4,c=this.attributes,l=0;l<4;l++)c.position.set(s+l,t);var h=c.texcoord;h.set(s,[i[0][0],i[0][1]]),h.set(s+1,[i[1][0],i[0][1]]),h.set(s+2,[i[1][0],i[1][1]]),h.set(s+3,[i[0][0],i[1][1]]),this.setSpriteAlign(e,n,r,o,a)}});Ee(jt.prototype,ht);const Ii=`@export ecgl.labels.vertex

attribute vec3 position: POSITION;
attribute vec2 texcoord: TEXCOORD_0;
attribute vec2 offset;
#ifdef VERTEX_COLOR
attribute vec4 a_Color : COLOR;
varying vec4 v_Color;
#endif

uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;
uniform vec4 viewport : VIEWPORT;

varying vec2 v_Texcoord;

void main()
{
 vec4 proj = worldViewProjection * vec4(position, 1.0);

 vec2 screen = (proj.xy / abs(proj.w) + 1.0) * 0.5 * viewport.zw;

 screen += offset;

 proj.xy = (screen / viewport.zw - 0.5) * 2.0 * abs(proj.w);
 gl_Position = proj;
#ifdef VERTEX_COLOR
 v_Color = a_Color;
#endif
 v_Texcoord = texcoord;
}
@end


@export ecgl.labels.fragment

uniform vec3 color : [1.0, 1.0, 1.0];
uniform float alpha : 1.0;
uniform sampler2D textureAtlas;
uniform vec2 uvScale: [1.0, 1.0];

#ifdef VERTEX_COLOR
varying vec4 v_Color;
#endif
varying float v_Miter;

varying vec2 v_Texcoord;

void main()
{
 gl_FragColor = vec4(color, alpha) * texture2D(textureAtlas, v_Texcoord * uvScale);
#ifdef VERTEX_COLOR
 gl_FragColor *= v_Color;
#endif
}

@end`;p.Shader.import(Ii);const Xt=p.Mesh.extend(function(){var e=new jt({dynamic:!0}),t=new p.Material({shader:p.createShader("ecgl.labels"),transparent:!0,depthMask:!1});return{geometry:e,material:t,culling:!1,castShadow:!1,ignorePicking:!0}});var Ke=je.vec3,wt=[[0,0],[1,1]],Xe=H.extend(function(){return{segmentScale:1,dynamic:!0,useNativeLine:!0,attributes:{position:new H.Attribute("position","float",3,"POSITION"),positionPrev:new H.Attribute("positionPrev","float",3),positionNext:new H.Attribute("positionNext","float",3),prevPositionPrev:new H.Attribute("prevPositionPrev","float",3),prevPosition:new H.Attribute("prevPosition","float",3),prevPositionNext:new H.Attribute("prevPositionNext","float",3),offset:new H.Attribute("offset","float",1),color:new H.Attribute("color","float",4,"COLOR")}}},{resetOffset:function(){this._vertexOffset=0,this._triangleOffset=0,this._itemVertexOffsets=[]},setVertexCount:function(e){var t=this.attributes;this.vertexCount!==e&&(t.position.init(e),t.color.init(e),this.useNativeLine||(t.positionPrev.init(e),t.positionNext.init(e),t.offset.init(e)),e>65535?this.indices instanceof Uint16Array&&(this.indices=new Uint32Array(this.indices)):this.indices instanceof Uint32Array&&(this.indices=new Uint16Array(this.indices)))},setTriangleCount:function(e){this.triangleCount!==e&&(e===0?this.indices=null:this.indices=this.vertexCount>65535?new Uint32Array(e*3):new Uint16Array(e*3))},_getCubicCurveApproxStep:function(e,t,n,i){var r=Ke.dist(e,t)+Ke.dist(n,t)+Ke.dist(i,n),o=1/(r+1)*this.segmentScale;return o},getCubicCurveVertexCount:function(e,t,n,i){var r=this._getCubicCurveApproxStep(e,t,n,i),o=Math.ceil(1/r);return this.useNativeLine?o*2:o*2+2},getCubicCurveTriangleCount:function(e,t,n,i){var r=this._getCubicCurveApproxStep(e,t,n,i),o=Math.ceil(1/r);return this.useNativeLine?0:o*2},getLineVertexCount:function(){return this.getPolylineVertexCount(wt)},getLineTriangleCount:function(){return this.getPolylineTriangleCount(wt)},getPolylineVertexCount:function(e){var t;if(typeof e=="number")t=e;else{var n=typeof e[0]!="number";t=n?e.length:e.length/3}return this.useNativeLine?(t-1)*2:(t-1)*2+2},getPolylineTriangleCount:function(e){var t;if(typeof e=="number")t=e;else{var n=typeof e[0]!="number";t=n?e.length:e.length/3}return this.useNativeLine?0:Math.max(t-1,0)*2},addCubicCurve:function(e,t,n,i,r,o){o==null&&(o=1);for(var a=e[0],s=e[1],c=e[2],l=t[0],h=t[1],f=t[2],d=n[0],u=n[1],m=n[2],v=i[0],_=i[1],g=i[2],x=this._getCubicCurveApproxStep(e,t,n,i),b=x*x,T=b*x,A=3*x,P=3*b,N=6*b,S=6*T,G=a-l*2+d,k=s-h*2+u,O=c-f*2+m,X=(l-d)*3-a+v,W=(h-u)*3-s+_,D=(f-m)*3-c+g,I=a,R=s,z=c,y=(l-a)*A+G*P+X*T,Y=(h-s)*A+k*P+W*T,w=(f-c)*A+O*P+D*T,q=G*N+X*S,B=k*N+W*S,K=O*N+D*S,Z=X*S,fe=W*S,_e=D*S,V=0,he=0,re=Math.ceil(1/x),j=new Float32Array((re+1)*3),j=[],ee=0,he=0;he<re+1;he++)j[ee++]=I,j[ee++]=R,j[ee++]=z,I+=y,R+=Y,z+=w,y+=q,Y+=B,w+=K,q+=Z,B+=fe,K+=_e,V+=x,V>1&&(I=y>0?Math.min(I,v):Math.max(I,v),R=Y>0?Math.min(R,_):Math.max(R,_),z=w>0?Math.min(z,g):Math.max(z,g));return this.addPolyline(j,r,o)},addLine:function(e,t,n,i){return this.addPolyline([e,t],n,i)},addPolyline:function(e,t,n,i,r){if(e.length){var o=typeof e[0]!="number";if(r==null&&(r=o?e.length:e.length/3),!(r<2)){i==null&&(i=0),n==null&&(n=1),this._itemVertexOffsets.push(this._vertexOffset);var o=typeof e[0]!="number",a=o?typeof t[0]!="number":t.length/4===r,s=this.attributes.position,c=this.attributes.positionPrev,l=this.attributes.positionNext,h=this.attributes.color,f=this.attributes.offset,d=this.indices,u=this._vertexOffset,m,v;n=Math.max(n,.01);for(var _=i;_<r;_++){if(o)m=e[_],a?v=t[_]:v=t;else{var g=_*3;if(m=m||[],m[0]=e[g],m[1]=e[g+1],m[2]=e[g+2],a){var x=_*4;v=v||[],v[0]=t[x],v[1]=t[x+1],v[2]=t[x+2],v[3]=t[x+3]}else v=t}if(this.useNativeLine?_>1&&(s.copy(u,u-1),h.copy(u,u-1),u++):(_<r-1&&(c.set(u+2,m),c.set(u+3,m)),_>0&&(l.set(u-2,m),l.set(u-1,m)),s.set(u,m),s.set(u+1,m),h.set(u,v),h.set(u+1,v),f.set(u,n/2),f.set(u+1,-n/2),u+=2),this.useNativeLine)h.set(u,v),s.set(u,m),u++;else if(_>0){var b=this._triangleOffset*3,d=this.indices;d[b]=u-4,d[b+1]=u-3,d[b+2]=u-2,d[b+3]=u-3,d[b+4]=u-1,d[b+5]=u-2,this._triangleOffset+=2}}if(!this.useNativeLine){var T=this._vertexOffset,A=this._vertexOffset+r*2;c.copy(T,T+2),c.copy(T+1,T+3),l.copy(A-1,A-3),l.copy(A-2,A-4)}return this._vertexOffset=u,this._vertexOffset}}},setItemColor:function(e,t){for(var n=this._itemVertexOffsets[e],i=e<this._itemVertexOffsets.length-1?this._itemVertexOffsets[e+1]:this._vertexOffset,r=n;r<i;r++)this.attributes.color.set(r,t);this.dirty("color")},currentTriangleOffset:function(){return this._triangleOffset},currentVertexOffset:function(){return this._vertexOffset}});Ee(Xe.prototype,ht);const Fi=`@export ecgl.lines3D.vertex

uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;

attribute vec3 position: POSITION;
attribute vec4 a_Color : COLOR;
varying vec4 v_Color;

void main()
{
 gl_Position = worldViewProjection * vec4(position, 1.0);
 v_Color = a_Color;
}

@end

@export ecgl.lines3D.fragment

uniform vec4 color : [1.0, 1.0, 1.0, 1.0];

varying vec4 v_Color;

@import clay.util.srgb

void main()
{
#ifdef SRGB_DECODE
 gl_FragColor = sRGBToLinear(color * v_Color);
#else
 gl_FragColor = color * v_Color;
#endif
}
@end



@export ecgl.lines3D.clipNear

vec4 clipNear(vec4 p1, vec4 p2) {
 float n = (p1.w - near) / (p1.w - p2.w);
 return vec4(mix(p1.xy, p2.xy, n), -near, near);
}

@end

@export ecgl.lines3D.expandLine
#ifdef VERTEX_ANIMATION
 vec4 prevProj = worldViewProjection * vec4(mix(prevPositionPrev, positionPrev, percent), 1.0);
 vec4 currProj = worldViewProjection * vec4(mix(prevPosition, position, percent), 1.0);
 vec4 nextProj = worldViewProjection * vec4(mix(prevPositionNext, positionNext, percent), 1.0);
#else
 vec4 prevProj = worldViewProjection * vec4(positionPrev, 1.0);
 vec4 currProj = worldViewProjection * vec4(position, 1.0);
 vec4 nextProj = worldViewProjection * vec4(positionNext, 1.0);
#endif

 if (currProj.w < 0.0) {
 if (nextProj.w > 0.0) {
 currProj = clipNear(currProj, nextProj);
 }
 else if (prevProj.w > 0.0) {
 currProj = clipNear(currProj, prevProj);
 }
 }

 vec2 prevScreen = (prevProj.xy / abs(prevProj.w) + 1.0) * 0.5 * viewport.zw;
 vec2 currScreen = (currProj.xy / abs(currProj.w) + 1.0) * 0.5 * viewport.zw;
 vec2 nextScreen = (nextProj.xy / abs(nextProj.w) + 1.0) * 0.5 * viewport.zw;

 vec2 dir;
 float len = offset;
 if (position == positionPrev) {
 dir = normalize(nextScreen - currScreen);
 }
 else if (position == positionNext) {
 dir = normalize(currScreen - prevScreen);
 }
 else {
 vec2 dirA = normalize(currScreen - prevScreen);
 vec2 dirB = normalize(nextScreen - currScreen);

 vec2 tanget = normalize(dirA + dirB);

 float miter = 1.0 / max(dot(tanget, dirA), 0.5);
 len *= miter;
 dir = tanget;
 }

 dir = vec2(-dir.y, dir.x) * len;
 currScreen += dir;

 currProj.xy = (currScreen / viewport.zw - 0.5) * 2.0 * abs(currProj.w);
@end


@export ecgl.meshLines3D.vertex

attribute vec3 position: POSITION;
attribute vec3 positionPrev;
attribute vec3 positionNext;
attribute float offset;
attribute vec4 a_Color : COLOR;

#ifdef VERTEX_ANIMATION
attribute vec3 prevPosition;
attribute vec3 prevPositionPrev;
attribute vec3 prevPositionNext;
uniform float percent : 1.0;
#endif

uniform mat4 worldViewProjection : WORLDVIEWPROJECTION;
uniform vec4 viewport : VIEWPORT;
uniform float near : NEAR;

varying vec4 v_Color;

@import ecgl.common.wireframe.vertexHeader

@import ecgl.lines3D.clipNear

void main()
{
 @import ecgl.lines3D.expandLine

 gl_Position = currProj;

 v_Color = a_Color;

 @import ecgl.common.wireframe.vertexMain
}
@end


@export ecgl.meshLines3D.fragment

uniform vec4 color : [1.0, 1.0, 1.0, 1.0];

varying vec4 v_Color;

@import ecgl.common.wireframe.fragmentHeader

@import clay.util.srgb

void main()
{
#ifdef SRGB_DECODE
 gl_FragColor = sRGBToLinear(color * v_Color);
#else
 gl_FragColor = color * v_Color;
#endif

 @import ecgl.common.wireframe.fragmentMain
}

@end`;var Wt=un.extend({type:"series.surface",dependencies:["globe","grid3D","geo3D"],visualStyleAccessPath:"itemStyle",formatTooltip:function(e){return Mi(this,e)},getInitialData:function(e,t){var n=e.data;function i(z){return!(isNaN(z.min)||isNaN(z.max)||isNaN(z.step))}function r(z){var y=Nt;return Math.max(y(z.min),y(z.max),y(z.step))+1}if(!n)if(e.parametric){var T=e.parametricEquation||{},A=T.u||{},P=T.v||{};["u","v"].forEach(function(y){i(T[y])}),["x","y","z"].forEach(function(y){T[y]});var N=Math.floor((A.max+A.step-A.min)/A.step),S=Math.floor((P.max+P.step-P.min)/P.step);n=new Float32Array(N*S*5);for(var G=r(A),k=r(P),d=0,u=0;u<S;u++)for(var m=0;m<N;m++){var O=m*A.step+A.min,X=u*P.step+P.min,W=De(Math.min(O,A.max),G),D=De(Math.min(X,P.max),k),v=T.x(W,D),_=T.y(W,D),b=T.z(W,D);n[d++]=v,n[d++]=_,n[d++]=b,n[d++]=W,n[d++]=D}}else{var o=e.equation||{},a=o.x||{},s=o.y||{};if(["x","y"].forEach(function(z){i(o[z])}),typeof o.z!="function")return;var c=Math.floor((a.max+a.step-a.min)/a.step),l=Math.floor((s.max+s.step-s.min)/s.step);n=new Float32Array(c*l*3);for(var h=r(a),f=r(s),d=0,u=0;u<l;u++)for(var m=0;m<c;m++){var v=m*a.step+a.min,_=u*s.step+s.min,g=De(Math.min(v,a.max),h),x=De(Math.min(_,s.max),f),b=o.z(g,x);n[d++]=g,n[d++]=x,n[d++]=b}}var I=["x","y","z"];e.parametric&&I.push("u","v");var R=Ri(this,I,n);return R},defaultOption:{coordinateSystem:"cartesian3D",zlevel:-10,grid3DIndex:0,shading:"lambert",parametric:!1,wireframe:{show:!0,lineStyle:{color:"rgba(0,0,0,0.5)",width:1}},equation:{x:{min:-1,max:1,step:.1},y:{min:-1,max:1,step:.1},z:null},parametricEquation:{u:{min:-1,max:1,step:.1},v:{min:-1,max:1,step:.1},x:null,y:null,z:null},dataShape:null,itemStyle:{},animationDurationUpdate:500}});ae(Wt.prototype,Ni);var ue=je.vec3;function zi(e){return isNaN(e[0])||isNaN(e[1])||isNaN(e[2])}const Bi=dn.extend({type:"surface",__ecgl__:!0,init:function(e,t){this.groupGL=new p.Node},render:function(e,t,n){var i=this._prevSurfaceMesh;this._prevSurfaceMesh=this._surfaceMesh,this._surfaceMesh=i,this._surfaceMesh||(this._surfaceMesh=this._createSurfaceMesh()),this.groupGL.remove(this._prevSurfaceMesh),this.groupGL.add(this._surfaceMesh);var r=e.coordinateSystem,o=e.get("shading"),a=e.getData(),s="ecgl."+o;if((!this._surfaceMesh.material||this._surfaceMesh.material.shader.name!==s)&&(this._surfaceMesh.material=p.createMaterial(s,["VERTEX_COLOR","DOUBLE_SIDED"])),p.setMaterialFromModel(o,this._surfaceMesh.material,e,n),r&&r.viewGL){r.viewGL.add(this.groupGL);var c=r.viewGL.isLinearSpace()?"define":"undefine";this._surfaceMesh.material[c]("fragment","SRGB_DECODE")}var l=e.get("parametric"),h=e.get("dataShape");h||(h=this._getDataShape(a,l));var f=e.getModel("wireframe"),d=f.get("lineStyle.width"),u=f.get("show")&&d>0;this._updateSurfaceMesh(this._surfaceMesh,e,h,u);var m=this._surfaceMesh.material;u?(m.define("WIREFRAME_QUAD"),m.set("wireframeLineWidth",d),m.set("wireframeLineColor",p.parseColor(f.get("lineStyle.color")))):m.undefine("WIREFRAME_QUAD"),this._initHandler(e,n),this._updateAnimation(e)},_updateAnimation:function(e){p.updateVertexAnimation([["prevPosition","position"],["prevNormal","normal"]],this._prevSurfaceMesh,this._surfaceMesh,e)},_createSurfaceMesh:function(){var e=new p.Mesh({geometry:new p.Geometry({dynamic:!0,sortTriangles:!0}),shadowDepthMaterial:new p.Material({shader:new p.Shader(p.Shader.source("ecgl.sm.depth.vertex"),p.Shader.source("ecgl.sm.depth.fragment"))}),culling:!1,renderOrder:10,renderNormal:!0});return e.geometry.createAttribute("barycentric","float",4),e.geometry.createAttribute("prevPosition","float",3),e.geometry.createAttribute("prevNormal","float",3),Object.assign(e.geometry,Oi),e},_initHandler:function(e,t){var n=e.getData(),i=this._surfaceMesh,r=e.coordinateSystem;function o(s,c){for(var l=1/0,h=-1,f=[],d=0;d<s.length;d++){i.geometry.attributes.position.get(s[d],f);var u=ue.dist(c.array,f);u<l&&(l=u,h=s[d])}return h}i.seriesIndex=e.seriesIndex;var a=-1;i.off("mousemove"),i.off("mouseout"),i.on("mousemove",function(s){var c=o(s.triangle,s.point);if(c>=0){var l=[];i.geometry.attributes.position.get(c,l);for(var h=r.pointToData(l),f=1/0,d=-1,u=[],m=0;m<n.count();m++){u[0]=n.get("x",m),u[1]=n.get("y",m),u[2]=n.get("z",m);var v=ue.squaredDistance(u,h);v<f&&(d=m,f=v)}d!==a&&t.dispatchAction({type:"grid3DShowAxisPointer",value:h}),a=d,i.dataIndex=d}else i.dataIndex=-1},this),i.on("mouseout",function(s){a=-1,i.dataIndex=-1,t.dispatchAction({type:"grid3DHideAxisPointer"})},this)},_updateSurfaceMesh:function(e,t,n,i){var r=e.geometry,o=t.getData(),a=o.getLayout("points"),s=0;o.each(function(ne){o.hasValue(ne)||s++});var c=s||i,l=r.attributes.position,h=r.attributes.normal,f=r.attributes.texcoord0,d=r.attributes.barycentric,u=r.attributes.color,m=n[0],v=n[1],_=t.get("shading"),g=_!=="color";if(c){var x=(m-1)*(v-1)*4;l.init(x),i&&d.init(x)}else l.value=new Float32Array(a);u.init(r.vertexCount),f.init(r.vertexCount);var b=[0,3,1,1,3,2],T=[[1,1,0,0],[0,1,0,1],[1,0,0,1],[1,0,1,0]],A=r.indices=new(r.vertexCount>65535?Uint32Array:Uint16Array)((m-1)*(v-1)*6),P=function(ne,ge,oe){oe[1]=ne*v+ge,oe[0]=ne*v+ge+1,oe[3]=(ne+1)*v+ge+1,oe[2]=(ne+1)*v+ge},N=!1;if(c){var S=[],G=[],k=0;g?h.init(r.vertexCount):h.value=null;for(var O=[[],[],[]],X=[],W=[],D=ue.create(),I=function(ne,ge,oe){var Ye=ge*3;return oe[0]=ne[Ye],oe[1]=ne[Ye+1],oe[2]=ne[Ye+2],oe},R=new Float32Array(a.length),z=new Float32Array(a.length/3*4),y=0;y<o.count();y++)if(o.hasValue(y)){var j=p.parseColor(ot(o,y)),Y=gt(o,y);Y!=null&&(j[3]*=Y),j[3]<.99&&(N=!0);for(var w=0;w<4;w++)z[y*4+w]=j[w]}for(var q=[1e7,1e7,1e7],y=0;y<m-1;y++)for(var B=0;B<v-1;B++){var K=y*(v-1)+B,Z=K*4;P(y,B,S);for(var fe=!1,w=0;w<4;w++)I(a,S[w],G),zi(G)&&(fe=!0);for(var w=0;w<4;w++)fe?l.set(Z+w,q):(I(a,S[w],G),l.set(Z+w,G)),i&&d.set(Z+w,T[w]);for(var w=0;w<6;w++)A[k++]=b[w]+Z;if(g&&!fe)for(var w=0;w<2;w++){for(var _e=w*3,V=0;V<3;V++){var he=S[b[_e]+V];I(a,he,O[V])}ue.sub(X,O[0],O[1]),ue.sub(W,O[1],O[2]),ue.cross(D,X,W);for(var V=0;V<3;V++){var re=S[b[_e]+V]*3;R[re]=R[re]+D[0],R[re+1]=R[re+1]+D[1],R[re+2]=R[re+2]+D[2]}}}if(g)for(var y=0;y<R.length/3;y++)I(R,y,D),ue.normalize(D,D),R[y*3]=D[0],R[y*3+1]=D[1],R[y*3+2]=D[2];for(var j=[],ee=[],y=0;y<m-1;y++)for(var B=0;B<v-1;B++){var K=y*(v-1)+B,Z=K*4;P(y,B,S);for(var w=0;w<4;w++){for(var V=0;V<4;V++)j[V]=z[S[w]*4+V];u.set(Z+w,j),g&&(I(R,S[w],D),h.set(Z+w,D));var he=S[w];ee[0]=he%v/(v-1),ee[1]=Math.floor(he/v)/(m-1),f.set(Z+w,ee)}K++}}else{for(var ee=[],y=0;y<o.count();y++){ee[0]=y%v/(v-1),ee[1]=Math.floor(y/v)/(m-1);var j=p.parseColor(ot(o,y)),Y=gt(o,y);Y!=null&&(j[3]*=Y),j[3]<.99&&(N=!0),u.set(y,j),f.set(y,ee)}for(var S=[],en=0,y=0;y<m-1;y++)for(var B=0;B<v-1;B++){P(y,B,S);for(var w=0;w<6;w++)A[en++]=S[b[w]]}g?r.generateVertexNormals():h.value=null}e.material.get("normalMap")&&r.generateTangents(),r.updateBoundingBox(),r.dirty(),e.material.transparent=N,e.material.depthMask=!N},_getDataShape:function(e,t){for(var n=-1/0,i=0,r=0,o=!1,a=t?"u":"x",s=e.count(),c=0;c<s;c++){var l=e.get(a,c);l<n&&(r=0,i++),n=l,r++}if((!i||r===1)&&(o=!0),!o)return[i+1,r];for(var h=Math.floor(Math.sqrt(s));h>0;){if(Math.floor(s/h)===s/h)return[h,s/h];h--}return h=Math.floor(Math.sqrt(s)),[h,h]},dispose:function(){this.groupGL.removeAll()},remove:function(){this.groupGL.removeAll()}});function hr(e){e.registerChartView(Bi),e.registerSeriesModel(Wt),e.registerLayout(function(t,n){t.eachSeriesByType("surface",function(i){var r=i.coordinateSystem;!r||r.type;var o=i.getData(),a=new Float32Array(3*o.count()),s=[NaN,NaN,NaN];if(r&&r.type==="cartesian3D"){var c=r.dimensions,l=c.map(function(h){return i.coordDimToDataDim(h)[0]});o.each(l,function(h,f,d,u){var m;o.hasValue(u)?m=r.dataToPoint([h,f,d]):m=s,a[u*3]=m[0],a[u*3+1]=m[1],a[u*3+2]=m[2]})}o.setLayout("points",a)})})}const Hi={defaultOption:{viewControl:{projection:"perspective",autoRotate:!1,autoRotateDirection:"cw",autoRotateSpeed:10,autoRotateAfterStill:3,damping:.8,rotateSensitivity:1,zoomSensitivity:1,panSensitivity:1,panMouseButton:"middle",rotateMouseButton:"left",distance:150,minDistance:40,maxDistance:400,orthographicSize:150,maxOrthographicSize:400,minOrthographicSize:20,center:[0,0,0],alpha:0,beta:0,minAlpha:-90,maxAlpha:90}},setView:function(e){e=e||{},this.option.viewControl=this.option.viewControl||{},e.alpha!=null&&(this.option.viewControl.alpha=e.alpha),e.beta!=null&&(this.option.viewControl.beta=e.beta),e.distance!=null&&(this.option.viewControl.distance=e.distance),e.center!=null&&(this.option.viewControl.center=e.center)}},Ui={defaultOption:{postEffect:{enable:!1,bloom:{enable:!0,intensity:.1},depthOfField:{enable:!1,focalRange:20,focalDistance:50,blurRadius:10,fstop:2.8,quality:"medium"},screenSpaceAmbientOcclusion:{enable:!1,radius:2,quality:"medium",intensity:1},screenSpaceReflection:{enable:!1,quality:"medium",maxRoughness:.8},colorCorrection:{enable:!0,exposure:0,brightness:0,contrast:1,saturation:1,lookupTexture:""},edge:{enable:!1},FXAA:{enable:!1}},temporalSuperSampling:{enable:"auto"}}},Gi={defaultOption:{light:{main:{shadow:!1,shadowQuality:"high",color:"#fff",intensity:1,alpha:0,beta:0},ambient:{color:"#fff",intensity:.2},ambientCubemap:{texture:null,exposure:1,diffuseIntensity:.5,specularIntensity:.5}}}};function be(e,t){for(var n=0,i=1/t,r=e;r>0;)n=n+i*(r%t),r=Math.floor(r/t),i=i/t;return n}const ki=`@export ecgl.ssao.estimate

uniform sampler2D depthTex;

uniform sampler2D normalTex;

uniform sampler2D noiseTex;

uniform vec2 depthTexSize;

uniform vec2 noiseTexSize;

uniform mat4 projection;

uniform mat4 projectionInv;

uniform mat4 viewInverseTranspose;

uniform vec3 kernel[KERNEL_SIZE];

uniform float radius : 1;

uniform float power : 1;

uniform float bias: 1e-2;

uniform float intensity: 1.0;

varying vec2 v_Texcoord;

float ssaoEstimator(in vec3 originPos, in mat3 kernelBasis) {
 float occlusion = 0.0;

 for (int i = 0; i < KERNEL_SIZE; i++) {
 vec3 samplePos = kernel[i];
#ifdef NORMALTEX_ENABLED
 samplePos = kernelBasis * samplePos;
#endif
 samplePos = samplePos * radius + originPos;

 vec4 texCoord = projection * vec4(samplePos, 1.0);
 texCoord.xy /= texCoord.w;

 vec4 depthTexel = texture2D(depthTex, texCoord.xy * 0.5 + 0.5);

 float sampleDepth = depthTexel.r * 2.0 - 1.0;
 if (projection[3][3] == 0.0) {
 sampleDepth = projection[3][2] / (sampleDepth * projection[2][3] - projection[2][2]);
 }
 else {
 sampleDepth = (sampleDepth - projection[3][2]) / projection[2][2];
 }
 
 float rangeCheck = smoothstep(0.0, 1.0, radius / abs(originPos.z - sampleDepth));
 occlusion += rangeCheck * step(samplePos.z, sampleDepth - bias);
 }
#ifdef NORMALTEX_ENABLED
 occlusion = 1.0 - occlusion / float(KERNEL_SIZE);
#else
 occlusion = 1.0 - clamp((occlusion / float(KERNEL_SIZE) - 0.6) * 2.5, 0.0, 1.0);
#endif
 return pow(occlusion, power);
}

void main()
{

 vec4 depthTexel = texture2D(depthTex, v_Texcoord);

#ifdef NORMALTEX_ENABLED
 vec4 tex = texture2D(normalTex, v_Texcoord);
 if (dot(tex.rgb, tex.rgb) == 0.0) {
 gl_FragColor = vec4(1.0);
 return;
 }
 vec3 N = tex.rgb * 2.0 - 1.0;
 N = (viewInverseTranspose * vec4(N, 0.0)).xyz;

 vec2 noiseTexCoord = depthTexSize / vec2(noiseTexSize) * v_Texcoord;
 vec3 rvec = texture2D(noiseTex, noiseTexCoord).rgb * 2.0 - 1.0;
 vec3 T = normalize(rvec - N * dot(rvec, N));
 vec3 BT = normalize(cross(N, T));
 mat3 kernelBasis = mat3(T, BT, N);
#else
 if (depthTexel.r > 0.99999) {
 gl_FragColor = vec4(1.0);
 return;
 }
 mat3 kernelBasis;
#endif

 float z = depthTexel.r * 2.0 - 1.0;

 vec4 projectedPos = vec4(v_Texcoord * 2.0 - 1.0, z, 1.0);
 vec4 p4 = projectionInv * projectedPos;

 vec3 position = p4.xyz / p4.w;

 float ao = ssaoEstimator(position, kernelBasis);
 ao = clamp(1.0 - (1.0 - ao) * intensity, 0.0, 1.0);
 gl_FragColor = vec4(vec3(ao), 1.0);
}

@end


@export ecgl.ssao.blur
#define SHADER_NAME SSAO_BLUR

uniform sampler2D ssaoTexture;

#ifdef NORMALTEX_ENABLED
uniform sampler2D normalTex;
#endif

varying vec2 v_Texcoord;

uniform vec2 textureSize;
uniform float blurSize : 1.0;

uniform int direction: 0.0;

#ifdef DEPTHTEX_ENABLED
uniform sampler2D depthTex;
uniform mat4 projection;
uniform float depthRange : 0.5;

float getLinearDepth(vec2 coord)
{
 float depth = texture2D(depthTex, coord).r * 2.0 - 1.0;
 return projection[3][2] / (depth * projection[2][3] - projection[2][2]);
}
#endif

void main()
{
 float kernel[5];
 kernel[0] = 0.122581;
 kernel[1] = 0.233062;
 kernel[2] = 0.288713;
 kernel[3] = 0.233062;
 kernel[4] = 0.122581;

 vec2 off = vec2(0.0);
 if (direction == 0) {
 off[0] = blurSize / textureSize.x;
 }
 else {
 off[1] = blurSize / textureSize.y;
 }

 vec2 coord = v_Texcoord;

 float sum = 0.0;
 float weightAll = 0.0;

#ifdef NORMALTEX_ENABLED
 vec3 centerNormal = texture2D(normalTex, v_Texcoord).rgb * 2.0 - 1.0;
#endif
#if defined(DEPTHTEX_ENABLED)
 float centerDepth = getLinearDepth(v_Texcoord);
#endif

 for (int i = 0; i < 5; i++) {
 vec2 coord = clamp(v_Texcoord + vec2(float(i) - 2.0) * off, vec2(0.0), vec2(1.0));

 float w = kernel[i];
#ifdef NORMALTEX_ENABLED
 vec3 normal = texture2D(normalTex, coord).rgb * 2.0 - 1.0;
 w *= clamp(dot(normal, centerNormal), 0.0, 1.0);
#endif
#ifdef DEPTHTEX_ENABLED
 float d = getLinearDepth(coord);
 w *= (1.0 - smoothstep(abs(centerDepth - d) / depthRange, 0.0, 1.0));
#endif

 weightAll += w;
 sum += texture2D(ssaoTexture, coord).r * w;
 }

 gl_FragColor = vec4(vec3(sum / weightAll), 1.0);
}

@end
`;L.import(ki);function Zt(e){for(var t=new Uint8Array(e*e*4),n=0,i=new Q,r=0;r<e;r++)for(var o=0;o<e;o++)i.set(Math.random()*2-1,Math.random()*2-1,0).normalize(),t[n++]=(i.x*.5+.5)*255,t[n++]=(i.y*.5+.5)*255,t[n++]=0,t[n++]=255;return t}function St(e){return new F({pixels:Zt(e),wrapS:U.REPEAT,wrapT:U.REPEAT,width:e,height:e})}function Vi(e,t,n){var i=new Float32Array(e*3);t=t||0;for(var r=0;r<e;r++){var o=be(r+t,2)*(n?1:2)*Math.PI,a=be(r+t,3)*Math.PI,s=Math.random(),c=Math.cos(o)*Math.sin(a)*s,l=Math.cos(a)*s,h=Math.sin(o)*Math.sin(a)*s;i[r*3]=c,i[r*3+1]=l,i[r*3+2]=h}return i}function le(e){e=e||{},this._ssaoPass=new ie({fragment:L.source("ecgl.ssao.estimate")}),this._blurPass=new ie({fragment:L.source("ecgl.ssao.blur")}),this._framebuffer=new se({depthBuffer:!1}),this._ssaoTexture=new F,this._blurTexture=new F,this._blurTexture2=new F,this._depthTex=e.depthTexture,this._normalTex=e.normalTexture,this.setNoiseSize(4),this.setKernelSize(e.kernelSize||12),e.radius!=null&&this.setParameter("radius",e.radius),e.power!=null&&this.setParameter("power",e.power),this._normalTex||(this._ssaoPass.material.disableTexture("normalTex"),this._blurPass.material.disableTexture("normalTex")),this._depthTex||this._blurPass.material.disableTexture("depthTex"),this._blurPass.material.setUniform("normalTex",this._normalTex),this._blurPass.material.setUniform("depthTex",this._depthTex)}le.prototype.setDepthTexture=function(e){this._depthTex=e};le.prototype.setNormalTexture=function(e){this._normalTex=e,this._ssaoPass.material[e?"enableTexture":"disableTexture"]("normalTex"),this.setKernelSize(this._kernelSize)};le.prototype.update=function(e,t,n){var i=e.getWidth(),r=e.getHeight(),o=this._ssaoPass,a=this._blurPass;o.setUniform("kernel",this._kernels[n%this._kernels.length]),o.setUniform("depthTex",this._depthTex),this._normalTex!=null&&o.setUniform("normalTex",this._normalTex),o.setUniform("depthTexSize",[this._depthTex.width,this._depthTex.height]);var s=new $;$.transpose(s,t.worldTransform),o.setUniform("projection",t.projectionMatrix.array),o.setUniform("projectionInv",t.invProjectionMatrix.array),o.setUniform("viewInverseTranspose",s.array);var c=this._ssaoTexture,l=this._blurTexture,h=this._blurTexture2;c.width=i/2,c.height=r/2,l.width=i,l.height=r,h.width=i,h.height=r,this._framebuffer.attach(c),this._framebuffer.bind(e),e.gl.clearColor(1,1,1,1),e.gl.clear(e.gl.COLOR_BUFFER_BIT),o.render(e),a.setUniform("textureSize",[i/2,r/2]),a.setUniform("projection",t.projectionMatrix.array),this._framebuffer.attach(l),a.setUniform("direction",0),a.setUniform("ssaoTexture",c),a.render(e),this._framebuffer.attach(h),a.setUniform("textureSize",[i,r]),a.setUniform("direction",1),a.setUniform("ssaoTexture",l),a.render(e),this._framebuffer.unbind(e);var f=e.clearColor;e.gl.clearColor(f[0],f[1],f[2],f[3])};le.prototype.getTargetTexture=function(){return this._blurTexture2};le.prototype.setParameter=function(e,t){e==="noiseTexSize"?this.setNoiseSize(t):e==="kernelSize"?this.setKernelSize(t):e==="intensity"?this._ssaoPass.material.set("intensity",t):this._ssaoPass.setUniform(e,t)};le.prototype.setKernelSize=function(e){this._kernelSize=e,this._ssaoPass.material.define("fragment","KERNEL_SIZE",e),this._kernels=this._kernels||[];for(var t=0;t<30;t++)this._kernels[t]=Vi(e,t*e,!!this._normalTex)};le.prototype.setNoiseSize=function(e){var t=this._ssaoPass.getUniform("noiseTex");t?(t.data=Zt(e),t.width=t.height=e,t.dirty()):(t=St(e),this._ssaoPass.setUniform("noiseTex",St(e))),this._ssaoPass.setUniform("noiseTexSize",[e,e])};le.prototype.dispose=function(e){this._blurTexture.dispose(e),this._ssaoTexture.dispose(e),this._blurTexture2.dispose(e)};const ji=`@export ecgl.ssr.main

#define SHADER_NAME SSR
#define MAX_ITERATION 20;
#define SAMPLE_PER_FRAME 5;
#define TOTAL_SAMPLES 128;

uniform sampler2D sourceTexture;
uniform sampler2D gBufferTexture1;
uniform sampler2D gBufferTexture2;
uniform sampler2D gBufferTexture3;
uniform samplerCube specularCubemap;
uniform float specularIntensity: 1;

uniform mat4 projection;
uniform mat4 projectionInv;
uniform mat4 toViewSpace;
uniform mat4 toWorldSpace;

uniform float maxRayDistance: 200;

uniform float pixelStride: 16;
uniform float pixelStrideZCutoff: 50; 
uniform float screenEdgeFadeStart: 0.9; 
uniform float eyeFadeStart : 0.2; uniform float eyeFadeEnd: 0.8; 
uniform float minGlossiness: 0.2; uniform float zThicknessThreshold: 1;

uniform float nearZ;
uniform vec2 viewportSize : VIEWPORT_SIZE;

uniform float jitterOffset: 0;

varying vec2 v_Texcoord;

#ifdef DEPTH_DECODE
@import clay.util.decode_float
#endif

#ifdef PHYSICALLY_CORRECT
uniform sampler2D normalDistribution;
uniform float sampleOffset: 0;
uniform vec2 normalDistributionSize;

vec3 transformNormal(vec3 H, vec3 N) {
 vec3 upVector = N.y > 0.999 ? vec3(1.0, 0.0, 0.0) : vec3(0.0, 1.0, 0.0);
 vec3 tangentX = normalize(cross(N, upVector));
 vec3 tangentZ = cross(N, tangentX);
 return normalize(tangentX * H.x + N * H.y + tangentZ * H.z);
}
vec3 importanceSampleNormalGGX(float i, float roughness, vec3 N) {
 float p = fract((i + sampleOffset) / float(TOTAL_SAMPLES));
 vec3 H = texture2D(normalDistribution,vec2(roughness, p)).rgb;
 return transformNormal(H, N);
}
float G_Smith(float g, float ndv, float ndl) {
 float roughness = 1.0 - g;
 float k = roughness * roughness / 2.0;
 float G1V = ndv / (ndv * (1.0 - k) + k);
 float G1L = ndl / (ndl * (1.0 - k) + k);
 return G1L * G1V;
}
vec3 F_Schlick(float ndv, vec3 spec) {
 return spec + (1.0 - spec) * pow(1.0 - ndv, 5.0);
}
#endif

float fetchDepth(sampler2D depthTexture, vec2 uv)
{
 vec4 depthTexel = texture2D(depthTexture, uv);
 return depthTexel.r * 2.0 - 1.0;
}

float linearDepth(float depth)
{
 if (projection[3][3] == 0.0) {
 return projection[3][2] / (depth * projection[2][3] - projection[2][2]);
 }
 else {
 return (depth - projection[3][2]) / projection[2][2];
 }
}

bool rayIntersectDepth(float rayZNear, float rayZFar, vec2 hitPixel)
{
 if (rayZFar > rayZNear)
 {
 float t = rayZFar; rayZFar = rayZNear; rayZNear = t;
 }
 float cameraZ = linearDepth(fetchDepth(gBufferTexture2, hitPixel));
 return rayZFar <= cameraZ && rayZNear >= cameraZ - zThicknessThreshold;
}


bool traceScreenSpaceRay(
 vec3 rayOrigin, vec3 rayDir, float jitter,
 out vec2 hitPixel, out vec3 hitPoint, out float iterationCount
)
{
 float rayLength = ((rayOrigin.z + rayDir.z * maxRayDistance) > -nearZ)
 ? (-nearZ - rayOrigin.z) / rayDir.z : maxRayDistance;

 vec3 rayEnd = rayOrigin + rayDir * rayLength;

 vec4 H0 = projection * vec4(rayOrigin, 1.0);
 vec4 H1 = projection * vec4(rayEnd, 1.0);

 float k0 = 1.0 / H0.w, k1 = 1.0 / H1.w;

 vec3 Q0 = rayOrigin * k0, Q1 = rayEnd * k1;

 vec2 P0 = (H0.xy * k0 * 0.5 + 0.5) * viewportSize;
 vec2 P1 = (H1.xy * k1 * 0.5 + 0.5) * viewportSize;

 P1 += dot(P1 - P0, P1 - P0) < 0.0001 ? 0.01 : 0.0;
 vec2 delta = P1 - P0;

 bool permute = false;
 if (abs(delta.x) < abs(delta.y)) {
 permute = true;
 delta = delta.yx;
 P0 = P0.yx;
 P1 = P1.yx;
 }
 float stepDir = sign(delta.x);
 float invdx = stepDir / delta.x;

 vec3 dQ = (Q1 - Q0) * invdx;
 float dk = (k1 - k0) * invdx;

 vec2 dP = vec2(stepDir, delta.y * invdx);

 float strideScaler = 1.0 - min(1.0, -rayOrigin.z / pixelStrideZCutoff);
 float pixStride = 1.0 + strideScaler * pixelStride;

 dP *= pixStride; dQ *= pixStride; dk *= pixStride;

 vec4 pqk = vec4(P0, Q0.z, k0);
 vec4 dPQK = vec4(dP, dQ.z, dk);

 pqk += dPQK * jitter;
 float rayZFar = (dPQK.z * 0.5 + pqk.z) / (dPQK.w * 0.5 + pqk.w);
 float rayZNear;

 bool intersect = false;

 vec2 texelSize = 1.0 / viewportSize;

 iterationCount = 0.0;

 for (int i = 0; i < MAX_ITERATION; i++)
 {
 pqk += dPQK;

 rayZNear = rayZFar;
 rayZFar = (dPQK.z * 0.5 + pqk.z) / (dPQK.w * 0.5 + pqk.w);

 hitPixel = permute ? pqk.yx : pqk.xy;
 hitPixel *= texelSize;

 intersect = rayIntersectDepth(rayZNear, rayZFar, hitPixel);

 iterationCount += 1.0;

 dPQK *= 1.2;

 if (intersect) {
 break;
 }
 }

 Q0.xy += dQ.xy * iterationCount;
 Q0.z = pqk.z;
 hitPoint = Q0 / pqk.w;

 return intersect;
}

float calculateAlpha(
 float iterationCount, float reflectivity,
 vec2 hitPixel, vec3 hitPoint, float dist, vec3 rayDir
)
{
 float alpha = clamp(reflectivity, 0.0, 1.0);
 alpha *= 1.0 - (iterationCount / float(MAX_ITERATION));
 vec2 hitPixelNDC = hitPixel * 2.0 - 1.0;
 float maxDimension = min(1.0, max(abs(hitPixelNDC.x), abs(hitPixelNDC.y)));
 alpha *= 1.0 - max(0.0, maxDimension - screenEdgeFadeStart) / (1.0 - screenEdgeFadeStart);

 float _eyeFadeStart = eyeFadeStart;
 float _eyeFadeEnd = eyeFadeEnd;
 if (_eyeFadeStart > _eyeFadeEnd) {
 float tmp = _eyeFadeEnd;
 _eyeFadeEnd = _eyeFadeStart;
 _eyeFadeStart = tmp;
 }

 float eyeDir = clamp(rayDir.z, _eyeFadeStart, _eyeFadeEnd);
 alpha *= 1.0 - (eyeDir - _eyeFadeStart) / (_eyeFadeEnd - _eyeFadeStart);

 alpha *= 1.0 - clamp(dist / maxRayDistance, 0.0, 1.0);

 return alpha;
}

@import clay.util.rand

@import clay.util.rgbm

void main()
{
 vec4 normalAndGloss = texture2D(gBufferTexture1, v_Texcoord);

 if (dot(normalAndGloss.rgb, vec3(1.0)) == 0.0) {
 discard;
 }

 float g = normalAndGloss.a;
#if !defined(PHYSICALLY_CORRECT)
 if (g <= minGlossiness) {
 discard;
 }
#endif

 float reflectivity = (g - minGlossiness) / (1.0 - minGlossiness);

 vec3 N = normalize(normalAndGloss.rgb * 2.0 - 1.0);
 N = normalize((toViewSpace * vec4(N, 0.0)).xyz);

 vec4 projectedPos = vec4(v_Texcoord * 2.0 - 1.0, fetchDepth(gBufferTexture2, v_Texcoord), 1.0);
 vec4 pos = projectionInv * projectedPos;
 vec3 rayOrigin = pos.xyz / pos.w;
 vec3 V = -normalize(rayOrigin);

 float ndv = clamp(dot(N, V), 0.0, 1.0);
 float iterationCount;
 float jitter = rand(fract(v_Texcoord + jitterOffset));

#ifdef PHYSICALLY_CORRECT
 vec4 color = vec4(vec3(0.0), 1.0);
 vec4 albedoMetalness = texture2D(gBufferTexture3, v_Texcoord);
 vec3 albedo = albedoMetalness.rgb;
 float m = albedoMetalness.a;
 vec3 diffuseColor = albedo * (1.0 - m);
 vec3 spec = mix(vec3(0.04), albedo, m);

 float jitter2 = rand(fract(v_Texcoord)) * float(TOTAL_SAMPLES);

 for (int i = 0; i < SAMPLE_PER_FRAME; i++) {
 vec3 H = importanceSampleNormalGGX(float(i) + jitter2, 1.0 - g, N);
 vec3 rayDir = normalize(reflect(-V, H));
#else
 vec3 rayDir = normalize(reflect(-V, N));
#endif
 vec2 hitPixel;
 vec3 hitPoint;

 bool intersect = traceScreenSpaceRay(rayOrigin, rayDir, jitter, hitPixel, hitPoint, iterationCount);

 float dist = distance(rayOrigin, hitPoint);

 vec3 hitNormal = texture2D(gBufferTexture1, hitPixel).rgb * 2.0 - 1.0;
 hitNormal = normalize((toViewSpace * vec4(hitNormal, 0.0)).xyz);
#ifdef PHYSICALLY_CORRECT
 float ndl = clamp(dot(N, rayDir), 0.0, 1.0);
 float vdh = clamp(dot(V, H), 0.0, 1.0);
 float ndh = clamp(dot(N, H), 0.0, 1.0);
 vec3 litTexel = vec3(0.0);
 if (dot(hitNormal, rayDir) < 0.0 && intersect) {
 litTexel = texture2D(sourceTexture, hitPixel).rgb;
 litTexel *= pow(clamp(1.0 - dist / 200.0, 0.0, 1.0), 3.0);

 }
 else {
 #ifdef SPECULARCUBEMAP_ENABLED
 vec3 rayDirW = normalize(toWorldSpace * vec4(rayDir, 0.0)).rgb;
 litTexel = RGBMDecode(textureCubeLodEXT(specularCubemap, rayDirW, 0.0), 8.12).rgb * specularIntensity;
#endif
 }
 color.rgb += ndl * litTexel * (
 F_Schlick(ndl, spec) * G_Smith(g, ndv, ndl) * vdh / (ndh * ndv + 0.001)
 );
 }
 color.rgb /= float(SAMPLE_PER_FRAME);
#else
 #if !defined(SPECULARCUBEMAP_ENABLED)
 if (dot(hitNormal, rayDir) >= 0.0) {
 discard;
 }
 if (!intersect) {
 discard;
 }
#endif
 float alpha = clamp(calculateAlpha(iterationCount, reflectivity, hitPixel, hitPoint, dist, rayDir), 0.0, 1.0);
 vec4 color = texture2D(sourceTexture, hitPixel);
 color.rgb *= alpha;

#ifdef SPECULARCUBEMAP_ENABLED
 vec3 rayDirW = normalize(toWorldSpace * vec4(rayDir, 0.0)).rgb;
 alpha = alpha * (intersect ? 1.0 : 0.0);
 float bias = (1.0 -g) * 5.0;
 color.rgb += (1.0 - alpha)
 * RGBMDecode(textureCubeLodEXT(specularCubemap, rayDirW, bias), 8.12).rgb
 * specularIntensity;
#endif

#endif

 gl_FragColor = encodeHDR(color);
}
@end

@export ecgl.ssr.blur

uniform sampler2D texture;
uniform sampler2D gBufferTexture1;
uniform sampler2D gBufferTexture2;
uniform mat4 projection;
uniform float depthRange : 0.05;

varying vec2 v_Texcoord;

uniform vec2 textureSize;
uniform float blurSize : 1.0;

#ifdef BLEND
 #ifdef SSAOTEX_ENABLED
uniform sampler2D ssaoTex;
 #endif
uniform sampler2D sourceTexture;
#endif

float getLinearDepth(vec2 coord)
{
 float depth = texture2D(gBufferTexture2, coord).r * 2.0 - 1.0;
 return projection[3][2] / (depth * projection[2][3] - projection[2][2]);
}

@import clay.util.rgbm


void main()
{
 @import clay.compositor.kernel.gaussian_9

 vec4 centerNTexel = texture2D(gBufferTexture1, v_Texcoord);
 float g = centerNTexel.a;
 float maxBlurSize = clamp(1.0 - g, 0.0, 1.0) * blurSize;
#ifdef VERTICAL
 vec2 off = vec2(0.0, maxBlurSize / textureSize.y);
#else
 vec2 off = vec2(maxBlurSize / textureSize.x, 0.0);
#endif

 vec2 coord = v_Texcoord;

 vec4 sum = vec4(0.0);
 float weightAll = 0.0;

 vec3 cN = centerNTexel.rgb * 2.0 - 1.0;
 float cD = getLinearDepth(v_Texcoord);
 for (int i = 0; i < 9; i++) {
 vec2 coord = clamp((float(i) - 4.0) * off + v_Texcoord, vec2(0.0), vec2(1.0));
 float w = gaussianKernel[i]
 * clamp(dot(cN, texture2D(gBufferTexture1, coord).rgb * 2.0 - 1.0), 0.0, 1.0);
 float d = getLinearDepth(coord);
 w *= (1.0 - smoothstep(abs(cD - d) / depthRange, 0.0, 1.0));

 weightAll += w;
 sum += decodeHDR(texture2D(texture, coord)) * w;
 }

#ifdef BLEND
 float aoFactor = 1.0;
 #ifdef SSAOTEX_ENABLED
 aoFactor = texture2D(ssaoTex, v_Texcoord).r;
 #endif
 gl_FragColor = encodeHDR(
 sum / weightAll * aoFactor + decodeHDR(texture2D(sourceTexture, v_Texcoord))
 );
#else
 gl_FragColor = encodeHDR(sum / weightAll);
#endif
}

@end`;L.import(ji);function ce(e){e=e||{},this._ssrPass=new ie({fragment:L.source("ecgl.ssr.main"),clearColor:[0,0,0,0]}),this._blurPass1=new ie({fragment:L.source("ecgl.ssr.blur"),clearColor:[0,0,0,0]}),this._blurPass2=new ie({fragment:L.source("ecgl.ssr.blur"),clearColor:[0,0,0,0]}),this._blendPass=new ie({fragment:L.source("clay.compositor.blend")}),this._blendPass.material.disableTexturesAll(),this._blendPass.material.enableTexture(["texture1","texture2"]),this._ssrPass.setUniform("gBufferTexture1",e.normalTexture),this._ssrPass.setUniform("gBufferTexture2",e.depthTexture),this._blurPass1.setUniform("gBufferTexture1",e.normalTexture),this._blurPass1.setUniform("gBufferTexture2",e.depthTexture),this._blurPass2.setUniform("gBufferTexture1",e.normalTexture),this._blurPass2.setUniform("gBufferTexture2",e.depthTexture),this._blurPass2.material.define("fragment","VERTICAL"),this._blurPass2.material.define("fragment","BLEND"),this._ssrTexture=new F({type:U.HALF_FLOAT}),this._texture2=new F({type:U.HALF_FLOAT}),this._texture3=new F({type:U.HALF_FLOAT}),this._prevTexture=new F({type:U.HALF_FLOAT}),this._currentTexture=new F({type:U.HALF_FLOAT}),this._frameBuffer=new se({depthBuffer:!1}),this._normalDistribution=null,this._totalSamples=256,this._samplePerFrame=4,this._ssrPass.material.define("fragment","SAMPLE_PER_FRAME",this._samplePerFrame),this._ssrPass.material.define("fragment","TOTAL_SAMPLES",this._totalSamples),this._downScale=1}ce.prototype.setAmbientCubemap=function(e,t){this._ssrPass.material.set("specularCubemap",e),this._ssrPass.material.set("specularIntensity",t);var n=e&&t;this._ssrPass.material[n?"enableTexture":"disableTexture"]("specularCubemap")};ce.prototype.update=function(e,t,n,i){var r=e.getWidth(),o=e.getHeight(),a=this._ssrTexture,s=this._texture2,c=this._texture3;a.width=this._prevTexture.width=this._currentTexture.width=r/this._downScale,a.height=this._prevTexture.height=this._currentTexture.height=o/this._downScale,s.width=c.width=r,s.height=c.height=o;var l=this._frameBuffer,h=this._ssrPass,f=this._blurPass1,d=this._blurPass2,u=this._blendPass,m=new $,v=new $;$.transpose(m,t.worldTransform),$.transpose(v,t.viewMatrix),h.setUniform("sourceTexture",n),h.setUniform("projection",t.projectionMatrix.array),h.setUniform("projectionInv",t.invProjectionMatrix.array),h.setUniform("toViewSpace",m.array),h.setUniform("toWorldSpace",v.array),h.setUniform("nearZ",t.near);var _=i/this._totalSamples*this._samplePerFrame;if(h.setUniform("jitterOffset",_),h.setUniform("sampleOffset",i*this._samplePerFrame),f.setUniform("textureSize",[a.width,a.height]),d.setUniform("textureSize",[r,o]),d.setUniform("sourceTexture",n),f.setUniform("projection",t.projectionMatrix.array),d.setUniform("projection",t.projectionMatrix.array),l.attach(a),l.bind(e),h.render(e),this._physicallyCorrect&&(l.attach(this._currentTexture),u.setUniform("texture1",this._prevTexture),u.setUniform("texture2",a),u.material.set({weight1:i>=1?.95:0,weight2:i>=1?.05:1}),u.render(e)),l.attach(s),f.setUniform("texture",this._physicallyCorrect?this._currentTexture:a),f.render(e),l.attach(c),d.setUniform("texture",s),d.render(e),l.unbind(e),this._physicallyCorrect){var g=this._prevTexture;this._prevTexture=this._currentTexture,this._currentTexture=g}};ce.prototype.getTargetTexture=function(){return this._texture3};ce.prototype.setParameter=function(e,t){e==="maxIteration"?this._ssrPass.material.define("fragment","MAX_ITERATION",t):this._ssrPass.setUniform(e,t)};ce.prototype.setPhysicallyCorrect=function(e){e?(this._normalDistribution||(this._normalDistribution=Kn.generateNormalDistribution(64,this._totalSamples)),this._ssrPass.material.define("fragment","PHYSICALLY_CORRECT"),this._ssrPass.material.set("normalDistribution",this._normalDistribution),this._ssrPass.material.set("normalDistributionSize",[64,this._totalSamples])):this._ssrPass.material.undefine("fragment","PHYSICALLY_CORRECT"),this._physicallyCorrect=e};ce.prototype.setSSAOTexture=function(e){var t=this._blurPass2;e?(t.material.enableTexture("ssaoTex"),t.material.set("ssaoTex",e)):t.material.disableTexture("ssaoTex")};ce.prototype.isFinished=function(e){return this._physicallyCorrect?e>this._totalSamples/this._samplePerFrame:!0};ce.prototype.dispose=function(e){this._ssrTexture.dispose(e),this._texture2.dispose(e),this._texture3.dispose(e),this._prevTexture.dispose(e),this._currentTexture.dispose(e),this._frameBuffer.dispose(e)};const At=[0,0,-.321585265978,-.154972575841,.458126042375,.188473391593,.842080129861,.527766490688,.147304551086,-.659453822776,-.331943915203,-.940619700594,.0479226680259,.54812163202,.701581552186,-.709825561388,-.295436780218,.940589268233,-.901489676764,.237713156085,.973570876096,-.109899459384,-.866792314779,-.451805525005,.330975007087,.800048655954,-.344275183665,.381779221166,-.386139432542,-.437418421534,-.576478634965,-.0148463392551,.385798197415,-.262426961053,-.666302061145,.682427250835,-.628010632582,-.732836215494,.10163141741,-.987658134403,.711995289051,-.320024291314,.0296005138058,.950296523438,.0130612307608,-.351024443122,-.879596633704,-.10478487883,.435712737232,.504254490347,.779203817497,.206477676721,.388264289969,-.896736162545,-.153106280781,-.629203242522,-.245517550697,.657969239148,.126830499058,.26862328493,-.634888119007,-.302301223431,.617074219636,.779817204925],Xi=`@export ecgl.normal.vertex

@import ecgl.common.transformUniforms

@import ecgl.common.uv.header

@import ecgl.common.attributes

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

@import ecgl.common.normalMap.vertexHeader

@import ecgl.common.vertexAnimation.header

void main()
{

 @import ecgl.common.vertexAnimation.main

 @import ecgl.common.uv.main

 v_Normal = normalize((worldInverseTranspose * vec4(normal, 0.0)).xyz);
 v_WorldPosition = (world * vec4(pos, 1.0)).xyz;

 @import ecgl.common.normalMap.vertexMain

 gl_Position = worldViewProjection * vec4(pos, 1.0);

}


@end


@export ecgl.normal.fragment

#define ROUGHNESS_CHANEL 0

uniform bool useBumpMap;
uniform bool useRoughnessMap;
uniform bool doubleSide;
uniform float roughness;

@import ecgl.common.uv.fragmentHeader

varying vec3 v_Normal;
varying vec3 v_WorldPosition;

uniform mat4 viewInverse : VIEWINVERSE;

@import ecgl.common.normalMap.fragmentHeader
@import ecgl.common.bumpMap.header

uniform sampler2D roughnessMap;

void main()
{
 vec3 N = v_Normal;
 
 bool flipNormal = false;
 if (doubleSide) {
 vec3 eyePos = viewInverse[3].xyz;
 vec3 V = normalize(eyePos - v_WorldPosition);

 if (dot(N, V) < 0.0) {
 flipNormal = true;
 }
 }

 @import ecgl.common.normalMap.fragmentMain

 if (useBumpMap) {
 N = bumpNormal(v_WorldPosition, v_Normal, N);
 }

 float g = 1.0 - roughness;

 if (useRoughnessMap) {
 float g2 = 1.0 - texture2D(roughnessMap, v_DetailTexcoord)[ROUGHNESS_CHANEL];
 g = clamp(g2 + (g - 0.5) * 2.0, 0.0, 1.0);
 }

 if (flipNormal) {
 N = -N;
 }

 gl_FragColor.rgb = (N.xyz + 1.0) * 0.5;
 gl_FragColor.a = g;
}
@end`;L.import(Xi);function Qe(e,t,n,i,r){var o=e.gl;t.setUniform(o,"1i",n,r),o.activeTexture(o.TEXTURE0+r),i.isRenderable()?i.bind(e):i.unbind(e)}function Wi(e,t,n,i,r){var o,a,s,c,l=e.gl;return function(h,f,d){if(!(c&&c.material===h.material)){var u=h.material,m=h.__program,v=u.get("roughness");v==null&&(v=1);var _=u.get("normalMap")||t,g=u.get("roughnessMap"),x=u.get("bumpMap"),b=u.get("uvRepeat"),T=u.get("uvOffset"),A=u.get("detailUvRepeat"),P=u.get("detailUvOffset"),N=!!x&&u.isTextureEnabled("bumpMap"),S=!!g&&u.isTextureEnabled("roughnessMap"),G=u.isDefined("fragment","DOUBLE_SIDED");x=x||n,g=g||i,d!==f?(f.set("normalMap",_),f.set("bumpMap",x),f.set("roughnessMap",g),f.set("useBumpMap",N),f.set("useRoughnessMap",S),f.set("doubleSide",G),b!=null&&f.set("uvRepeat",b),T!=null&&f.set("uvOffset",T),A!=null&&f.set("detailUvRepeat",A),P!=null&&f.set("detailUvOffset",P),f.set("roughness",v)):(m.setUniform(l,"1f","roughness",v),o!==_&&Qe(e,m,"normalMap",_,0),a!==x&&x&&Qe(e,m,"bumpMap",x,1),s!==g&&g&&Qe(e,m,"roughnessMap",g,2),b!=null&&m.setUniform(l,"2f","uvRepeat",b),T!=null&&m.setUniform(l,"2f","uvOffset",T),A!=null&&m.setUniform(l,"2f","detailUvRepeat",A),P!=null&&m.setUniform(l,"2f","detailUvOffset",P),m.setUniform(l,"1i","useBumpMap",+N),m.setUniform(l,"1i","useRoughnessMap",+S),m.setUniform(l,"1i","doubleSide",+G)),o=_,a=x,s=g,c=h}}}function Te(e){this._depthTex=new F({format:U.DEPTH_COMPONENT,type:U.UNSIGNED_INT}),this._normalTex=new F({type:U.HALF_FLOAT}),this._framebuffer=new se,this._framebuffer.attach(this._normalTex),this._framebuffer.attach(this._depthTex,se.DEPTH_ATTACHMENT),this._normalMaterial=new Ve({shader:new L(L.source("ecgl.normal.vertex"),L.source("ecgl.normal.fragment"))}),this._normalMaterial.enableTexture(["normalMap","bumpMap","roughnessMap"]),this._defaultNormalMap=xe.createBlank("#000"),this._defaultBumpMap=xe.createBlank("#000"),this._defaultRoughessMap=xe.createBlank("#000"),this._debugPass=new ie({fragment:L.source("clay.compositor.output")}),this._debugPass.setUniform("texture",this._normalTex),this._debugPass.material.undefine("fragment","OUTPUT_ALPHA")}Te.prototype.getDepthTexture=function(){return this._depthTex};Te.prototype.getNormalTexture=function(){return this._normalTex};Te.prototype.update=function(e,t,n){var i=e.getWidth(),r=e.getHeight(),o=this._depthTex,a=this._normalTex,s=this._normalMaterial;o.width=i,o.height=r,a.width=i,a.height=r;var c=t.getRenderList(n).opaque;this._framebuffer.bind(e),e.gl.clearColor(0,0,0,0),e.gl.clear(e.gl.COLOR_BUFFER_BIT|e.gl.DEPTH_BUFFER_BIT),e.gl.disable(e.gl.BLEND),e.renderPass(c,n,{getMaterial:function(){return s},ifRender:function(l){return l.renderNormal},beforeRender:Wi(e,this._defaultNormalMap,this._defaultBumpMap,this._defaultRoughessMap,this._normalMaterial),sort:e.opaqueSortCompare}),this._framebuffer.unbind(e)};Te.prototype.renderDebug=function(e){this._debugPass.render(e)};Te.prototype.dispose=function(e){this._depthTex.dispose(e),this._normalTex.dispose(e)};function Ne(e){e=e||{},this._edgePass=new ie({fragment:L.source("ecgl.edge")}),this._edgePass.setUniform("normalTexture",e.normalTexture),this._edgePass.setUniform("depthTexture",e.depthTexture),this._targetTexture=new F({type:U.HALF_FLOAT}),this._frameBuffer=new se,this._frameBuffer.attach(this._targetTexture)}Ne.prototype.update=function(e,t,n,i){var r=e.getWidth(),o=e.getHeight(),a=this._targetTexture;a.width=r,a.height=o;var s=this._frameBuffer;s.bind(e),this._edgePass.setUniform("projectionInv",t.invProjectionMatrix.array),this._edgePass.setUniform("textureSize",[r,o]),this._edgePass.setUniform("texture",n),this._edgePass.render(e),s.unbind(e)};Ne.prototype.getTargetTexture=function(){return this._targetTexture};Ne.prototype.setParameter=function(e,t){this._edgePass.setUniform(e,t)};Ne.prototype.dispose=function(e){this._targetTexture.dispose(e),this._frameBuffer.dispose(e)};const Zi={nodes:[{name:"source",type:"texture",outputs:{color:{}}},{name:"source_half",shader:"#source(clay.compositor.downsample)",inputs:{texture:"source"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 2)",height:"expr(height * 1.0 / 2)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0, height * 1.0] )"}},{name:"bright",shader:"#source(clay.compositor.bright)",inputs:{texture:"source_half"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 2)",height:"expr(height * 1.0 / 2)",type:"HALF_FLOAT"}}},parameters:{threshold:2,scale:4,textureSize:"expr([width * 1.0 / 2, height / 2])"}},{name:"bright_downsample_4",shader:"#source(clay.compositor.downsample)",inputs:{texture:"bright"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 4)",height:"expr(height * 1.0 / 4)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0 / 2, height / 2] )"}},{name:"bright_downsample_8",shader:"#source(clay.compositor.downsample)",inputs:{texture:"bright_downsample_4"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 8)",height:"expr(height * 1.0 / 8)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0 / 4, height / 4] )"}},{name:"bright_downsample_16",shader:"#source(clay.compositor.downsample)",inputs:{texture:"bright_downsample_8"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 16)",height:"expr(height * 1.0 / 16)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0 / 8, height / 8] )"}},{name:"bright_downsample_32",shader:"#source(clay.compositor.downsample)",inputs:{texture:"bright_downsample_16"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 32)",height:"expr(height * 1.0 / 32)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0 / 16, height / 16] )"}},{name:"bright_upsample_16_blur_h",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_downsample_32"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 16)",height:"expr(height * 1.0 / 16)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:0,textureSize:"expr( [width * 1.0 / 32, height / 32] )"}},{name:"bright_upsample_16_blur_v",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_upsample_16_blur_h"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 16)",height:"expr(height * 1.0 / 16)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:1,textureSize:"expr( [width * 1.0 / 16, height * 1.0 / 16] )"}},{name:"bright_upsample_8_blur_h",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_downsample_16"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 8)",height:"expr(height * 1.0 / 8)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:0,textureSize:"expr( [width * 1.0 / 16, height * 1.0 / 16] )"}},{name:"bright_upsample_8_blur_v",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_upsample_8_blur_h"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 8)",height:"expr(height * 1.0 / 8)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:1,textureSize:"expr( [width * 1.0 / 8, height * 1.0 / 8] )"}},{name:"bright_upsample_8_blend",shader:"#source(clay.compositor.blend)",inputs:{texture1:"bright_upsample_8_blur_v",texture2:"bright_upsample_16_blur_v"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 8)",height:"expr(height * 1.0 / 8)",type:"HALF_FLOAT"}}},parameters:{weight1:.3,weight2:.7}},{name:"bright_upsample_4_blur_h",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_downsample_8"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 4)",height:"expr(height * 1.0 / 4)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:0,textureSize:"expr( [width * 1.0 / 8, height * 1.0 / 8] )"}},{name:"bright_upsample_4_blur_v",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_upsample_4_blur_h"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 4)",height:"expr(height * 1.0 / 4)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:1,textureSize:"expr( [width * 1.0 / 4, height * 1.0 / 4] )"}},{name:"bright_upsample_4_blend",shader:"#source(clay.compositor.blend)",inputs:{texture1:"bright_upsample_4_blur_v",texture2:"bright_upsample_8_blend"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 4)",height:"expr(height * 1.0 / 4)",type:"HALF_FLOAT"}}},parameters:{weight1:.3,weight2:.7}},{name:"bright_upsample_2_blur_h",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_downsample_4"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 2)",height:"expr(height * 1.0 / 2)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:0,textureSize:"expr( [width * 1.0 / 4, height * 1.0 / 4] )"}},{name:"bright_upsample_2_blur_v",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_upsample_2_blur_h"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 2)",height:"expr(height * 1.0 / 2)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:1,textureSize:"expr( [width * 1.0 / 2, height * 1.0 / 2] )"}},{name:"bright_upsample_2_blend",shader:"#source(clay.compositor.blend)",inputs:{texture1:"bright_upsample_2_blur_v",texture2:"bright_upsample_4_blend"},outputs:{color:{parameters:{width:"expr(width * 1.0 / 2)",height:"expr(height * 1.0 / 2)",type:"HALF_FLOAT"}}},parameters:{weight1:.3,weight2:.7}},{name:"bright_upsample_full_blur_h",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:0,textureSize:"expr( [width * 1.0 / 2, height * 1.0 / 2] )"}},{name:"bright_upsample_full_blur_v",shader:"#source(clay.compositor.gaussian_blur)",inputs:{texture:"bright_upsample_full_blur_h"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}},parameters:{blurSize:1,blurDir:1,textureSize:"expr( [width * 1.0, height * 1.0] )"}},{name:"bloom_composite",shader:"#source(clay.compositor.blend)",inputs:{texture1:"bright_upsample_full_blur_v",texture2:"bright_upsample_2_blend"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}},parameters:{weight1:.3,weight2:.7}},{name:"coc",shader:"#source(ecgl.dof.coc)",outputs:{color:{parameters:{minFilter:"NEAREST",magFilter:"NEAREST",width:"expr(width * 1.0)",height:"expr(height * 1.0)"}}},parameters:{focalDist:50,focalRange:30}},{name:"dof_far_blur",shader:"#source(ecgl.dof.diskBlur)",inputs:{texture:"source",coc:"coc"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0, height * 1.0] )"}},{name:"dof_near_blur",shader:"#source(ecgl.dof.diskBlur)",inputs:{texture:"source",coc:"coc"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}},parameters:{textureSize:"expr( [width * 1.0, height * 1.0] )"},defines:{BLUR_NEARFIELD:null}},{name:"dof_coc_blur",shader:"#source(ecgl.dof.diskBlur)",inputs:{texture:"coc"},outputs:{color:{parameters:{minFilter:"NEAREST",magFilter:"NEAREST",width:"expr(width * 1.0)",height:"expr(height * 1.0)"}}},parameters:{textureSize:"expr( [width * 1.0, height * 1.0] )"},defines:{BLUR_COC:null}},{name:"dof_composite",shader:"#source(ecgl.dof.composite)",inputs:{original:"source",blurred:"dof_far_blur",nearfield:"dof_near_blur",coc:"coc",nearcoc:"dof_coc_blur"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)",type:"HALF_FLOAT"}}}},{name:"composite",shader:"#source(clay.compositor.hdr.composite)",inputs:{texture:"source",bloom:"bloom_composite"},outputs:{color:{parameters:{width:"expr(width * 1.0)",height:"expr(height * 1.0)"}}},defines:{}},{name:"FXAA",shader:"#source(clay.compositor.fxaa)",inputs:{texture:"composite"}}]},Yi=`@export ecgl.dof.coc

uniform sampler2D depth;

uniform float zNear: 0.1;
uniform float zFar: 2000;

uniform float focalDistance: 3;
uniform float focalRange: 1;
uniform float focalLength: 30;
uniform float fstop: 2.8;

varying vec2 v_Texcoord;

@import clay.util.encode_float

void main()
{
 float z = texture2D(depth, v_Texcoord).r * 2.0 - 1.0;

 float dist = 2.0 * zNear * zFar / (zFar + zNear - z * (zFar - zNear));

 float aperture = focalLength / fstop;

 float coc;

 float uppper = focalDistance + focalRange;
 float lower = focalDistance - focalRange;
 if (dist <= uppper && dist >= lower) {
 coc = 0.5;
 }
 else {
 float focalAdjusted = dist > uppper ? uppper : lower;

 coc = abs(aperture * (focalLength * (dist - focalAdjusted)) / (dist * (focalAdjusted - focalLength)));
 coc = clamp(coc, 0.0, 2.0) / 2.00001;

 if (dist < lower) {
 coc = -coc;
 }
 coc = coc * 0.5 + 0.5;
 }

 gl_FragColor = encodeFloat(coc);
}
@end


@export ecgl.dof.composite

#define DEBUG 0

uniform sampler2D original;
uniform sampler2D blurred;
uniform sampler2D nearfield;
uniform sampler2D coc;
uniform sampler2D nearcoc;
varying vec2 v_Texcoord;

@import clay.util.rgbm
@import clay.util.float

void main()
{
 vec4 blurredColor = texture2D(blurred, v_Texcoord);
 vec4 originalColor = texture2D(original, v_Texcoord);

 float fCoc = decodeFloat(texture2D(coc, v_Texcoord));

 fCoc = abs(fCoc * 2.0 - 1.0);

 float weight = smoothstep(0.0, 1.0, fCoc);
 
#ifdef NEARFIELD_ENABLED
 vec4 nearfieldColor = texture2D(nearfield, v_Texcoord);
 float fNearCoc = decodeFloat(texture2D(nearcoc, v_Texcoord));
 fNearCoc = abs(fNearCoc * 2.0 - 1.0);

 gl_FragColor = encodeHDR(
 mix(
 nearfieldColor, mix(originalColor, blurredColor, weight),
 pow(1.0 - fNearCoc, 4.0)
 )
 );
#else
 gl_FragColor = encodeHDR(mix(originalColor, blurredColor, weight));
#endif

}

@end



@export ecgl.dof.diskBlur

#define POISSON_KERNEL_SIZE 16;

uniform sampler2D texture;
uniform sampler2D coc;
varying vec2 v_Texcoord;

uniform float blurRadius : 10.0;
uniform vec2 textureSize : [512.0, 512.0];

uniform vec2 poissonKernel[POISSON_KERNEL_SIZE];

uniform float percent;

float nrand(const in vec2 n) {
 return fract(sin(dot(n.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

@import clay.util.rgbm
@import clay.util.float


void main()
{
 vec2 offset = blurRadius / textureSize;

 float rnd = 6.28318 * nrand(v_Texcoord + 0.07 * percent );
 float cosa = cos(rnd);
 float sina = sin(rnd);
 vec4 basis = vec4(cosa, -sina, sina, cosa);

#if !defined(BLUR_NEARFIELD) && !defined(BLUR_COC)
 offset *= abs(decodeFloat(texture2D(coc, v_Texcoord)) * 2.0 - 1.0);
#endif

#ifdef BLUR_COC
 float cocSum = 0.0;
#else
 vec4 color = vec4(0.0);
#endif


 float weightSum = 0.0;

 for (int i = 0; i < POISSON_KERNEL_SIZE; i++) {
 vec2 ofs = poissonKernel[i];

 ofs = vec2(dot(ofs, basis.xy), dot(ofs, basis.zw));

 vec2 uv = v_Texcoord + ofs * offset;
 vec4 texel = texture2D(texture, uv);

 float w = 1.0;
#ifdef BLUR_COC
 float fCoc = decodeFloat(texel) * 2.0 - 1.0;
 cocSum += clamp(fCoc, -1.0, 0.0) * w;
#else
 texel = texel;
 #if !defined(BLUR_NEARFIELD)
 float fCoc = decodeFloat(texture2D(coc, uv)) * 2.0 - 1.0;
 w *= abs(fCoc);
 #endif
 texel.rgb *= texel.a;
 color += texel * w;
#endif

 weightSum += w;
 }

#ifdef BLUR_COC
 gl_FragColor = encodeFloat(clamp(cocSum / weightSum, -1.0, 0.0) * 0.5 + 0.5);
#else
 color /= weightSum;
 color.rgb /= (color.a + 0.0001);
 gl_FragColor = color;
#endif
}

@end`,qi=`@export ecgl.edge

uniform sampler2D texture;

uniform sampler2D normalTexture;
uniform sampler2D depthTexture;

uniform mat4 projectionInv;

uniform vec2 textureSize;

uniform vec4 edgeColor: [0,0,0,0.8];

varying vec2 v_Texcoord;

vec3 packColor(vec2 coord) {
 float z = texture2D(depthTexture, coord).r * 2.0 - 1.0;
 vec4 p = vec4(v_Texcoord * 2.0 - 1.0, z, 1.0);
 vec4 p4 = projectionInv * p;

 return vec3(
 texture2D(normalTexture, coord).rg,
 -p4.z / p4.w / 5.0
 );
}

void main() {
 vec2 cc = v_Texcoord;
 vec3 center = packColor(cc);

 float size = clamp(1.0 - (center.z - 10.0) / 100.0, 0.0, 1.0) * 0.5;
 float dx = size / textureSize.x;
 float dy = size / textureSize.y;

 vec2 coord;
 vec3 topLeft = packColor(cc+vec2(-dx, -dy));
 vec3 top = packColor(cc+vec2(0.0, -dy));
 vec3 topRight = packColor(cc+vec2(dx, -dy));
 vec3 left = packColor(cc+vec2(-dx, 0.0));
 vec3 right = packColor(cc+vec2(dx, 0.0));
 vec3 bottomLeft = packColor(cc+vec2(-dx, dy));
 vec3 bottom = packColor(cc+vec2(0.0, dy));
 vec3 bottomRight = packColor(cc+vec2(dx, dy));

 vec3 v = -topLeft-2.0*top-topRight+bottomLeft+2.0*bottom+bottomRight;
 vec3 h = -bottomLeft-2.0*left-topLeft+bottomRight+2.0*right+topRight;

 float edge = sqrt(dot(h, h) + dot(v, v));

 edge = smoothstep(0.8, 1.0, edge);

 gl_FragColor = mix(texture2D(texture, v_Texcoord), vec4(edgeColor.rgb, 1.0), edgeColor.a * edge);
}
@end`;L.import($n);L.import(Jn);L.import(ei);L.import(ti);L.import(ni);L.import(ii);L.import(ri);L.import(oi);L.import(ai);L.import(Yi);L.import(qi);function Yt(e,t){return{color:{parameters:{width:e,height:t}}}}var ft=["composite","FXAA"];function C(){this._width,this._height,this._dpr,this._sourceTexture=new F({type:U.HALF_FLOAT}),this._depthTexture=new F({format:U.DEPTH_COMPONENT,type:U.UNSIGNED_INT}),this._framebuffer=new se,this._framebuffer.attach(this._sourceTexture),this._framebuffer.attach(this._depthTexture,se.DEPTH_ATTACHMENT),this._normalPass=new Te,this._compositor=Qn(Zi);var e=this._compositor.getNodeByName("source");e.texture=this._sourceTexture;var t=this._compositor.getNodeByName("coc");this._sourceNode=e,this._cocNode=t,this._compositeNode=this._compositor.getNodeByName("composite"),this._fxaaNode=this._compositor.getNodeByName("FXAA"),this._dofBlurNodes=["dof_far_blur","dof_near_blur","dof_coc_blur"].map(function(i){return this._compositor.getNodeByName(i)},this),this._dofBlurKernel=0,this._dofBlurKernelSize=new Float32Array(0),this._finalNodesChain=ft.map(function(i){return this._compositor.getNodeByName(i)},this);var n={normalTexture:this._normalPass.getNormalTexture(),depthTexture:this._normalPass.getDepthTexture()};this._ssaoPass=new le(n),this._ssrPass=new ce(n),this._edgePass=new Ne(n)}C.prototype.resize=function(i,r,n){n=n||1;var i=i*n,r=r*n,o=this._sourceTexture,a=this._depthTexture;o.width=i,o.height=r,a.width=i,a.height=r;var s={getWidth:function(){return i},getHeight:function(){return r},getDevicePixelRatio:function(){return n}};function c(l,h){if(typeof l[h]=="function"){var f=l[h].__original||l[h];l[h]=function(d){return f.call(this,s)},l[h].__original=f}}this._compositor.nodes.forEach(function(l){for(var h in l.outputs){var f=l.outputs[h].parameters;f&&(c(f,"width"),c(f,"height"))}for(var d in l.parameters)c(l.parameters,d)}),this._width=i,this._height=r,this._dpr=n};C.prototype.getWidth=function(){return this._width};C.prototype.getHeight=function(){return this._height};C.prototype._ifRenderNormalPass=function(){return this._enableSSAO||this._enableEdge||this._enableSSR};C.prototype._getPrevNode=function(e){for(var t=ft.indexOf(e.name)-1,n=this._finalNodesChain[t];n&&!this._compositor.getNodeByName(n.name);)t-=1,n=this._finalNodesChain[t];return n};C.prototype._getNextNode=function(e){for(var t=ft.indexOf(e.name)+1,n=this._finalNodesChain[t];n&&!this._compositor.getNodeByName(n.name);)t+=1,n=this._finalNodesChain[t];return n};C.prototype._addChainNode=function(e){var t=this._getPrevNode(e),n=this._getNextNode(e);t&&(e.inputs.texture=t.name,n?(e.outputs=Yt(this.getWidth.bind(this),this.getHeight.bind(this)),n.inputs.texture=e.name):e.outputs=null,this._compositor.addNode(e))};C.prototype._removeChainNode=function(e){var t=this._getPrevNode(e),n=this._getNextNode(e);t&&(n?(t.outputs=Yt(this.getWidth.bind(this),this.getHeight.bind(this)),n.inputs.texture=t.name):t.outputs=null,this._compositor.removeNode(e))};C.prototype.updateNormal=function(e,t,n,i){this._ifRenderNormalPass()&&this._normalPass.update(e,t,n)};C.prototype.updateSSAO=function(e,t,n,i){this._ssaoPass.update(e,n,i)};C.prototype.enableSSAO=function(){this._enableSSAO=!0};C.prototype.disableSSAO=function(){this._enableSSAO=!1};C.prototype.enableSSR=function(){this._enableSSR=!0};C.prototype.disableSSR=function(){this._enableSSR=!1};C.prototype.getSSAOTexture=function(){return this._ssaoPass.getTargetTexture()};C.prototype.getSourceFrameBuffer=function(){return this._framebuffer};C.prototype.getSourceTexture=function(){return this._sourceTexture};C.prototype.disableFXAA=function(){this._removeChainNode(this._fxaaNode)};C.prototype.enableFXAA=function(){this._addChainNode(this._fxaaNode)};C.prototype.enableBloom=function(){this._compositeNode.inputs.bloom="bloom_composite",this._compositor.dirty()};C.prototype.disableBloom=function(){this._compositeNode.inputs.bloom=null,this._compositor.dirty()};C.prototype.enableDOF=function(){this._compositeNode.inputs.texture="dof_composite",this._compositor.dirty()};C.prototype.disableDOF=function(){this._compositeNode.inputs.texture="source",this._compositor.dirty()};C.prototype.enableColorCorrection=function(){this._compositeNode.define("COLOR_CORRECTION"),this._enableColorCorrection=!0};C.prototype.disableColorCorrection=function(){this._compositeNode.undefine("COLOR_CORRECTION"),this._enableColorCorrection=!1};C.prototype.enableEdge=function(){this._enableEdge=!0};C.prototype.disableEdge=function(){this._enableEdge=!1};C.prototype.setBloomIntensity=function(e){this._compositeNode.setParameter("bloomIntensity",e)};C.prototype.setSSAOParameter=function(e,t){switch(e){case"quality":var n={low:6,medium:12,high:32,ultra:62}[t]||12;this._ssaoPass.setParameter("kernelSize",n);break;case"radius":this._ssaoPass.setParameter(e,t),this._ssaoPass.setParameter("bias",t/200);break;case"intensity":this._ssaoPass.setParameter(e,t);break}};C.prototype.setDOFParameter=function(e,t){switch(e){case"focalDistance":case"focalRange":case"fstop":this._cocNode.setParameter(e,t);break;case"blurRadius":for(var n=0;n<this._dofBlurNodes.length;n++)this._dofBlurNodes[n].setParameter("blurRadius",t);break;case"quality":var i={low:4,medium:8,high:16,ultra:32}[t]||8;this._dofBlurKernelSize=i;for(var n=0;n<this._dofBlurNodes.length;n++)this._dofBlurNodes[n].pass.material.define("POISSON_KERNEL_SIZE",i);this._dofBlurKernel=new Float32Array(i*2);break}};C.prototype.setSSRParameter=function(e,t){if(t!=null)switch(e){case"quality":var n={low:10,medium:15,high:30,ultra:80}[t]||20,i={low:32,medium:16,high:8,ultra:4}[t]||16;this._ssrPass.setParameter("maxIteration",n),this._ssrPass.setParameter("pixelStride",i);break;case"maxRoughness":this._ssrPass.setParameter("minGlossiness",Math.max(Math.min(1-t,1),0));break;case"physical":this.setPhysicallyCorrectSSR(t);break;default:console.warn("Unkown SSR parameter "+e)}};C.prototype.setPhysicallyCorrectSSR=function(e){this._ssrPass.setPhysicallyCorrect(e)};C.prototype.setEdgeColor=function(e){var t=p.parseColor(e);this._edgePass.setParameter("edgeColor",t)};C.prototype.setExposure=function(e){this._compositeNode.setParameter("exposure",Math.pow(2,e))};C.prototype.setColorLookupTexture=function(e,t){this._compositeNode.pass.material.setTextureImage("lut",this._enableColorCorrection?e:"none",t,{minFilter:p.Texture.NEAREST,magFilter:p.Texture.NEAREST,flipY:!1})};C.prototype.setColorCorrection=function(e,t){this._compositeNode.setParameter(e,t)};C.prototype.isSSREnabled=function(){return this._enableSSR};C.prototype.composite=function(e,t,n,i,r){var o=this._sourceTexture,a=o;this._enableEdge&&(this._edgePass.update(e,n,o,r),o=a=this._edgePass.getTargetTexture()),this._enableSSR&&(this._ssrPass.update(e,n,o,r),a=this._ssrPass.getTargetTexture(),this._ssrPass.setSSAOTexture(this._enableSSAO?this._ssaoPass.getTargetTexture():null)),this._sourceNode.texture=a,this._cocNode.setParameter("depth",this._depthTexture);for(var s=this._dofBlurKernel,c=this._dofBlurKernelSize,l=Math.floor(At.length/2/c),h=r%l,f=0;f<c*2;f++)s[f]=At[f+h*c*2];for(var f=0;f<this._dofBlurNodes.length;f++)this._dofBlurNodes[f].setParameter("percent",r/30),this._dofBlurNodes[f].setParameter("poissonKernel",s);this._cocNode.setParameter("zNear",n.near),this._cocNode.setParameter("zFar",n.far),this._compositor.render(e,i)};C.prototype.dispose=function(e){this._sourceTexture.dispose(e),this._depthTexture.dispose(e),this._framebuffer.dispose(e),this._compositor.dispose(e),this._normalPass.dispose(e),this._ssaoPass.dispose(e)};function st(e){for(var t=[],n=0;n<30;n++)t.push([be(n,2),be(n,3)]);this._haltonSequence=t,this._frame=0,this._sourceTex=new F,this._sourceFb=new se,this._sourceFb.attach(this._sourceTex),this._prevFrameTex=new F,this._outputTex=new F;var i=this._blendPass=new ie({fragment:L.source("clay.compositor.blend")});i.material.disableTexturesAll(),i.material.enableTexture(["texture1","texture2"]),this._blendFb=new se({depthBuffer:!1}),this._outputPass=new ie({fragment:L.source("clay.compositor.output"),blendWithPrevious:!0}),this._outputPass.material.define("fragment","OUTPUT_ALPHA"),this._outputPass.material.blend=function(r){r.blendEquationSeparate(r.FUNC_ADD,r.FUNC_ADD),r.blendFuncSeparate(r.ONE,r.ONE_MINUS_SRC_ALPHA,r.ONE,r.ONE_MINUS_SRC_ALPHA)}}st.prototype={constructor:st,jitterProjection:function(e,t){var n=e.viewport,i=n.devicePixelRatio||e.getDevicePixelRatio(),r=n.width*i,o=n.height*i,a=this._haltonSequence[this._frame%this._haltonSequence.length],s=new $;s.array[12]=(a[0]*2-1)/r,s.array[13]=(a[1]*2-1)/o,$.mul(t.projectionMatrix,s,t.projectionMatrix),$.invert(t.invProjectionMatrix,t.projectionMatrix)},resetFrame:function(){this._frame=0},getFrame:function(){return this._frame},getSourceFrameBuffer:function(){return this._sourceFb},getOutputTexture:function(){return this._outputTex},resize:function(e,t){this._prevFrameTex.width=e,this._prevFrameTex.height=t,this._outputTex.width=e,this._outputTex.height=t,this._sourceTex.width=e,this._sourceTex.height=t,this._prevFrameTex.dirty(),this._outputTex.dirty(),this._sourceTex.dirty()},isFinished:function(){return this._frame>=this._haltonSequence.length},render:function(e,t,n){var i=this._blendPass;this._frame===0?(i.setUniform("weight1",0),i.setUniform("weight2",1)):(i.setUniform("weight1",.9),i.setUniform("weight2",.1)),i.setUniform("texture1",this._prevFrameTex),i.setUniform("texture2",t||this._sourceTex),this._blendFb.attach(this._outputTex),this._blendFb.bind(e),i.render(e),this._blendFb.unbind(e),n||(this._outputPass.setUniform("texture",this._outputTex),this._outputPass.render(e));var r=this._prevFrameTex;this._prevFrameTex=this._outputTex,this._outputTex=r,this._frame++},dispose:function(e){this._sourceFb.dispose(e),this._blendFb.dispose(e),this._prevFrameTex.dispose(e),this._outputTex.dispose(e),this._sourceTex.dispose(e),this._outputPass.dispose(e),this._blendPass.dispose(e)}};function M(e){e=e||"perspective",this.layer=null,this.scene=new Pe,this.rootNode=this.scene,this.viewport={x:0,y:0,width:0,height:0},this.setProjection(e),this._compositor=new C,this._temporalSS=new st,this._shadowMapPass=new si;for(var t=[],n=0,i=0;i<30;i++){for(var r=[],o=0;o<6;o++)r.push(be(n,2)*4-2),r.push(be(n,3)*4-2),n++;t.push(r)}this._pcfKernels=t,this.scene.on("beforerender",function(a,s,c){this.needsTemporalSS()&&this._temporalSS.jitterProjection(a,c)},this)}M.prototype.setProjection=function(e){var t=this.camera;t&&t.update(),e==="perspective"?this.camera instanceof Fe||(this.camera=new Fe,t&&this.camera.setLocalTransform(t.localTransform)):this.camera instanceof et||(this.camera=new et,t&&this.camera.setLocalTransform(t.localTransform)),this.camera.near=.1,this.camera.far=2e3};M.prototype.setViewport=function(e,t,n,i,r){this.camera instanceof Fe&&(this.camera.aspect=n/i),r=r||1,this.viewport.x=e,this.viewport.y=t,this.viewport.width=n,this.viewport.height=i,this.viewport.devicePixelRatio=r,this._compositor.resize(n*r,i*r),this._temporalSS.resize(n*r,i*r)};M.prototype.containPoint=function(e,t){var n=this.viewport,i=this.layer.renderer.getHeight();return t=i-t,e>=n.x&&t>=n.y&&e<=n.x+n.width&&t<=n.y+n.height};var Ct=new te;M.prototype.castRay=function(e,t,n){var i=this.layer.renderer,r=i.viewport;return i.viewport=this.viewport,i.screenToNDC(e,t,Ct),this.camera.castRay(Ct,n),i.viewport=r,n};M.prototype.prepareRender=function(){this.scene.update(),this.camera.update(),this.scene.updateLights();var e=this.scene.updateRenderList(this.camera);this._needsSortProgressively=!1;for(var t=0;t<e.transparent.length;t++){var n=e.transparent[t],i=n.geometry;i.needsSortVerticesProgressively&&i.needsSortVerticesProgressively()&&(this._needsSortProgressively=!0),i.needsSortTrianglesProgressively&&i.needsSortTrianglesProgressively()&&(this._needsSortProgressively=!0)}this._frame=0,this._temporalSS.resetFrame()};M.prototype.render=function(e,t){this._doRender(e,t,this._frame),this._frame++};M.prototype.needsAccumulate=function(){return this.needsTemporalSS()||this._needsSortProgressively};M.prototype.needsTemporalSS=function(){var e=this._enableTemporalSS;return e==="auto"&&(e=this._enablePostEffect),e};M.prototype.hasDOF=function(){return this._enableDOF};M.prototype.isAccumulateFinished=function(){return this.needsTemporalSS()?this._temporalSS.isFinished():this._frame>30};M.prototype._doRender=function(e,t,n){var i=this.scene,r=this.camera;n=n||0,this._updateTransparent(e,i,r,n),t||(this._shadowMapPass.kernelPCF=this._pcfKernels[0],this._shadowMapPass.render(e,i,r,!0)),this._updateShadowPCFKernel(n);var o=e.clearColor;if(e.gl.clearColor(o[0],o[1],o[2],o[3]),this._enablePostEffect&&(this.needsTemporalSS()&&this._temporalSS.jitterProjection(e,r),this._compositor.updateNormal(e,i,r,this._temporalSS.getFrame())),this._updateSSAO(e,i,r,this._temporalSS.getFrame()),this._enablePostEffect){var a=this._compositor.getSourceFrameBuffer();a.bind(e),e.gl.clear(e.gl.DEPTH_BUFFER_BIT|e.gl.COLOR_BUFFER_BIT),e.render(i,r,!0,!0),a.unbind(e),this.needsTemporalSS()&&t?(this._compositor.composite(e,i,r,this._temporalSS.getSourceFrameBuffer(),this._temporalSS.getFrame()),e.setViewport(this.viewport),this._temporalSS.render(e)):(e.setViewport(this.viewport),this._compositor.composite(e,i,r,null,0))}else if(this.needsTemporalSS()&&t){var a=this._temporalSS.getSourceFrameBuffer();a.bind(e),e.saveClear(),e.clearBit=e.gl.DEPTH_BUFFER_BIT|e.gl.COLOR_BUFFER_BIT,e.render(i,r,!0,!0),e.restoreClear(),a.unbind(e),e.setViewport(this.viewport),this._temporalSS.render(e)}else e.setViewport(this.viewport),e.render(i,r,!0,!0)};M.prototype._updateTransparent=function(e,t,n,i){for(var r=new Q,o=new $,a=n.getWorldPosition(),s=t.getRenderList(n).transparent,c=0;c<s.length;c++){var l=s[c],h=l.geometry;$.invert(o,l.worldTransform),Q.transformMat4(r,a,o),h.needsSortTriangles&&h.needsSortTriangles()&&h.doSortTriangles(r,i),h.needsSortVertices&&h.needsSortVertices()&&h.doSortVertices(r,i)}};M.prototype._updateSSAO=function(e,t,n){var i=this._enableSSAO&&this._enablePostEffect;i&&this._compositor.updateSSAO(e,t,n,this._temporalSS.getFrame());for(var r=t.getRenderList(n),o=0;o<r.opaque.length;o++){var a=r.opaque[o];a.renderNormal&&a.material[i?"enableTexture":"disableTexture"]("ssaoMap"),i&&a.material.set("ssaoMap",this._compositor.getSSAOTexture())}};M.prototype._updateShadowPCFKernel=function(e){for(var t=this._pcfKernels[e%this._pcfKernels.length],n=this.scene.getRenderList(this.camera),i=n.opaque,r=0;r<i.length;r++)i[r].receiveShadow&&(i[r].material.set("pcfKernel",t),i[r].material.define("fragment","PCF_KERNEL_SIZE",t.length/2))};M.prototype.dispose=function(e){this._compositor.dispose(e.gl),this._temporalSS.dispose(e.gl),this._shadowMapPass.dispose(e)};M.prototype.setPostEffect=function(e,t){var n=this._compositor;this._enablePostEffect=e.get("enable");var i=e.getModel("bloom"),r=e.getModel("edge"),o=e.getModel("DOF",e.getModel("depthOfField")),a=e.getModel("SSAO",e.getModel("screenSpaceAmbientOcclusion")),s=e.getModel("SSR",e.getModel("screenSpaceReflection")),c=e.getModel("FXAA"),l=e.getModel("colorCorrection");i.get("enable")?n.enableBloom():n.disableBloom(),o.get("enable")?n.enableDOF():n.disableDOF(),s.get("enable")?n.enableSSR():n.disableSSR(),l.get("enable")?n.enableColorCorrection():n.disableColorCorrection(),r.get("enable")?n.enableEdge():n.disableEdge(),c.get("enable")?n.enableFXAA():n.disableFXAA(),this._enableDOF=o.get("enable"),this._enableSSAO=a.get("enable"),this._enableSSAO?n.enableSSAO():n.disableSSAO(),n.setBloomIntensity(i.get("intensity")),n.setEdgeColor(r.get("color")),n.setColorLookupTexture(l.get("lookupTexture"),t),n.setExposure(l.get("exposure")),["radius","quality","intensity"].forEach(function(h){n.setSSAOParameter(h,a.get(h))}),["quality","maxRoughness","physical"].forEach(function(h){n.setSSRParameter(h,s.get(h))}),["quality","focalDistance","focalRange","blurRadius","fstop"].forEach(function(h){n.setDOFParameter(h,o.get(h))}),["brightness","contrast","saturation"].forEach(function(h){n.setColorCorrection(h,l.get(h))})};M.prototype.setDOFFocusOnPoint=function(e){if(this._enablePostEffect)return e>this.camera.far||e<this.camera.near?void 0:(this._compositor.setDOFParameter("focalDistance",e),!0)};M.prototype.setTemporalSuperSampling=function(e){this._enableTemporalSS=e.get("enable")};M.prototype.isLinearSpace=function(){return this._enablePostEffect};M.prototype.setRootNode=function(e){if(this.rootNode!==e){for(var t=this.rootNode.children(),n=0;n<t.length;n++)e.add(t[n]);e!==this.scene&&this.scene.add(e),this.rootNode=e}};M.prototype.add=function(e){this.rootNode.add(e)};M.prototype.remove=function(e){this.rootNode.remove(e)};M.prototype.removeAll=function(e){this.rootNode.removeAll(e)};Object.assign(M.prototype,Bt);var we=J.firstNotNull,Lt={left:0,middle:1,right:2};function Pt(e){return e instanceof Array||(e=[e,e]),e}var qt=li.extend(function(){return{zr:null,viewGL:null,_center:new Q,minDistance:.5,maxDistance:1.5,maxOrthographicSize:300,minOrthographicSize:30,minAlpha:-90,maxAlpha:90,minBeta:-1/0,maxBeta:1/0,autoRotateAfterStill:0,autoRotateDirection:"cw",autoRotateSpeed:60,damping:.8,rotateSensitivity:1,zoomSensitivity:1,panSensitivity:1,panMouseButton:"middle",rotateMouseButton:"left",_mode:"rotate",_camera:null,_needsUpdate:!1,_rotating:!1,_phi:0,_theta:0,_mouseX:0,_mouseY:0,_rotateVelocity:new te,_panVelocity:new te,_distance:500,_zoomSpeed:0,_stillTimeout:0,_animators:[]}},function(){["_mouseDownHandler","_mouseWheelHandler","_mouseMoveHandler","_mouseUpHandler","_pinchHandler","_contextMenuHandler","_update"].forEach(function(e){this[e]=this[e].bind(this)},this)},{init:function(){var e=this.zr;e&&(e.on("mousedown",this._mouseDownHandler),e.on("globalout",this._mouseUpHandler),e.on("mousewheel",this._mouseWheelHandler),e.on("pinch",this._pinchHandler),e.animation.on("frame",this._update),e.dom.addEventListener("contextmenu",this._contextMenuHandler))},dispose:function(){var e=this.zr;e&&(e.off("mousedown",this._mouseDownHandler),e.off("mousemove",this._mouseMoveHandler),e.off("mouseup",this._mouseUpHandler),e.off("mousewheel",this._mouseWheelHandler),e.off("pinch",this._pinchHandler),e.off("globalout",this._mouseUpHandler),e.dom.removeEventListener("contextmenu",this._contextMenuHandler),e.animation.off("frame",this._update)),this.stopAllAnimation()},getDistance:function(){return this._distance},setDistance:function(e){this._distance=e,this._needsUpdate=!0},getOrthographicSize:function(){return this._orthoSize},setOrthographicSize:function(e){this._orthoSize=e,this._needsUpdate=!0},getAlpha:function(){return this._theta/Math.PI*180},getBeta:function(){return-this._phi/Math.PI*180},getCenter:function(){return this._center.toArray()},setAlpha:function(e){e=Math.max(Math.min(this.maxAlpha,e),this.minAlpha),this._theta=e/180*Math.PI,this._needsUpdate=!0},setBeta:function(e){e=Math.max(Math.min(this.maxBeta,e),this.minBeta),this._phi=-e/180*Math.PI,this._needsUpdate=!0},setCenter:function(e){this._center.setArray(e)},setViewGL:function(e){this.viewGL=e},getCamera:function(){return this.viewGL.camera},setFromViewControlModel:function(e,t){t=t||{};var n=t.baseDistance||0,i=t.baseOrthoSize||1,r=e.get("projection");r!=="perspective"&&r!=="orthographic"&&r!=="isometric"&&(r="perspective"),this._projection=r,this.viewGL.setProjection(r);var o=e.get("distance")+n,a=e.get("orthographicSize")+i;[["damping",.8],["autoRotate",!1],["autoRotateAfterStill",3],["autoRotateDirection","cw"],["autoRotateSpeed",10],["minDistance",30],["maxDistance",400],["minOrthographicSize",30],["maxOrthographicSize",300],["minAlpha",-90],["maxAlpha",90],["minBeta",-1/0],["maxBeta",1/0],["rotateSensitivity",1],["zoomSensitivity",1],["panSensitivity",1],["panMouseButton","left"],["rotateMouseButton","middle"]].forEach(function(d){this[d[0]]=we(e.get(d[0]),d[1])},this),this.minDistance+=n,this.maxDistance+=n,this.minOrthographicSize+=i,this.maxOrthographicSize+=i;var s=e.ecModel,c={};["animation","animationDurationUpdate","animationEasingUpdate"].forEach(function(d){c[d]=we(e.get(d),s&&s.get(d))});var l=we(t.alpha,e.get("alpha"))||0,h=we(t.beta,e.get("beta"))||0,f=we(t.center,e.get("center"))||[0,0,0];c.animation&&c.animationDurationUpdate>0&&this._notFirst?this.animateTo({alpha:l,beta:h,center:f,distance:o,orthographicSize:a,easing:c.animationEasingUpdate,duration:c.animationDurationUpdate}):(this.setDistance(o),this.setAlpha(l),this.setBeta(h),this.setCenter(f),this.setOrthographicSize(a)),this._notFirst=!0,this._validateProperties()},_validateProperties:function(){},animateTo:function(e){var t=this.zr,n=this,i={},r={};return e.distance!=null&&(i.distance=this.getDistance(),r.distance=e.distance),e.orthographicSize!=null&&(i.orthographicSize=this.getOrthographicSize(),r.orthographicSize=e.orthographicSize),e.alpha!=null&&(i.alpha=this.getAlpha(),r.alpha=e.alpha),e.beta!=null&&(i.beta=this.getBeta(),r.beta=e.beta),e.center!=null&&(i.center=this.getCenter(),r.center=e.center),this._addAnimator(t.animation.animate(i).when(e.duration||1e3,r).during(function(){i.alpha!=null&&n.setAlpha(i.alpha),i.beta!=null&&n.setBeta(i.beta),i.distance!=null&&n.setDistance(i.distance),i.center!=null&&n.setCenter(i.center),i.orthographicSize!=null&&n.setOrthographicSize(i.orthographicSize),n._needsUpdate=!0})).start(e.easing||"linear")},stopAllAnimation:function(){for(var e=0;e<this._animators.length;e++)this._animators[e].stop();this._animators.length=0},update:function(){this._needsUpdate=!0,this._update(20)},_isAnimating:function(){return this._animators.length>0},_update:function(e){if(this._rotating){var t=(this.autoRotateDirection==="cw"?1:-1)*this.autoRotateSpeed/180*Math.PI;this._phi-=t*e/1e3,this._needsUpdate=!0}else this._rotateVelocity.len()>0&&(this._needsUpdate=!0);(Math.abs(this._zoomSpeed)>.1||this._panVelocity.len()>0)&&(this._needsUpdate=!0),this._needsUpdate&&(e=Math.min(e,50),this._updateDistanceOrSize(e),this._updatePan(e),this._updateRotate(e),this._updateTransform(),this.getCamera().update(),this.zr&&this.zr.refresh(),this.trigger("update"),this._needsUpdate=!1)},_updateRotate:function(e){var t=this._rotateVelocity;this._phi=t.y*e/20+this._phi,this._theta=t.x*e/20+this._theta,this.setAlpha(this.getAlpha()),this.setBeta(this.getBeta()),this._vectorDamping(t,Math.pow(this.damping,e/16))},_updateDistanceOrSize:function(e){this._projection==="perspective"?this._setDistance(this._distance+this._zoomSpeed*e/20):this._setOrthoSize(this._orthoSize+this._zoomSpeed*e/20),this._zoomSpeed*=Math.pow(this.damping,e/16)},_setDistance:function(e){this._distance=Math.max(Math.min(e,this.maxDistance),this.minDistance)},_setOrthoSize:function(e){this._orthoSize=Math.max(Math.min(e,this.maxOrthographicSize),this.minOrthographicSize);var t=this.getCamera(),n=this._orthoSize,i=n/this.viewGL.viewport.height*this.viewGL.viewport.width;t.left=-i/2,t.right=i/2,t.top=n/2,t.bottom=-n/2},_updatePan:function(e){var t=this._panVelocity,n=this._distance,i=this.getCamera(),r=i.worldTransform.y,o=i.worldTransform.x;this._center.scaleAndAdd(o,-t.x*n/200).scaleAndAdd(r,-t.y*n/200),this._vectorDamping(t,0)},_updateTransform:function(){var e=this.getCamera(),t=new Q,n=this._theta+Math.PI/2,i=this._phi+Math.PI/2,r=Math.sin(n);t.x=r*Math.cos(i),t.y=-Math.cos(n),t.z=r*Math.sin(i),e.position.copy(this._center).scaleAndAdd(t,this._distance),e.rotation.identity().rotateY(-this._phi).rotateX(-this._theta)},_startCountingStill:function(){clearTimeout(this._stillTimeout);var e=this.autoRotateAfterStill,t=this;!isNaN(e)&&e>0&&(this._stillTimeout=setTimeout(function(){t._rotating=!0},e*1e3))},_vectorDamping:function(e,t){var n=e.len();n=n*t,n<1e-4&&(n=0),e.normalize().scale(n)},_decomposeTransform:function(){if(this.getCamera()){this.getCamera().updateWorldTransform();var e=this.getCamera().worldTransform.z,t=Math.asin(e.y),n=Math.atan2(e.x,e.z);this._theta=t,this._phi=-n,this.setBeta(this.getBeta()),this.setAlpha(this.getAlpha()),this.getCamera().aspect?this._setDistance(this.getCamera().position.dist(this._center)):this._setOrthoSize(this.getCamera().top-this.getCamera().bottom)}},_mouseDownHandler:function(e){if(!e.target&&!this._isAnimating()){var t=e.offsetX,n=e.offsetY;this.viewGL&&!this.viewGL.containPoint(t,n)||(this.zr.on("mousemove",this._mouseMoveHandler),this.zr.on("mouseup",this._mouseUpHandler),e.event.targetTouches?e.event.targetTouches.length===1&&(this._mode="rotate"):e.event.button===Lt[this.rotateMouseButton]?this._mode="rotate":e.event.button===Lt[this.panMouseButton]?this._mode="pan":this._mode="",this._rotateVelocity.set(0,0),this._rotating=!1,this.autoRotate&&this._startCountingStill(),this._mouseX=e.offsetX,this._mouseY=e.offsetY)}},_mouseMoveHandler:function(e){if(!(e.target&&e.target.__isGLToZRProxy)&&!this._isAnimating()){var t=Pt(this.panSensitivity),n=Pt(this.rotateSensitivity);this._mode==="rotate"?(this._rotateVelocity.y=(e.offsetX-this._mouseX)/this.zr.getHeight()*2*n[0],this._rotateVelocity.x=(e.offsetY-this._mouseY)/this.zr.getWidth()*2*n[1]):this._mode==="pan"&&(this._panVelocity.x=(e.offsetX-this._mouseX)/this.zr.getWidth()*t[0]*400,this._panVelocity.y=(-e.offsetY+this._mouseY)/this.zr.getHeight()*t[1]*400),this._mouseX=e.offsetX,this._mouseY=e.offsetY,e.event.preventDefault()}},_mouseWheelHandler:function(e){if(!this._isAnimating()){var t=e.event.wheelDelta||-e.event.detail;this._zoomHandler(e,t)}},_pinchHandler:function(e){this._isAnimating()||(this._zoomHandler(e,e.pinchScale>1?1:-1),this._mode="")},_zoomHandler:function(e,t){if(t!==0){var n=e.offsetX,i=e.offsetY;if(!(this.viewGL&&!this.viewGL.containPoint(n,i))){var r;this._projection==="perspective"?r=Math.max(Math.max(Math.min(this._distance-this.minDistance,this.maxDistance-this._distance))/20,.5):r=Math.max(Math.max(Math.min(this._orthoSize-this.minOrthographicSize,this.maxOrthographicSize-this._orthoSize))/20,.5),this._zoomSpeed=(t>0?-1:1)*r*this.zoomSensitivity,this._rotating=!1,this.autoRotate&&this._mode==="rotate"&&this._startCountingStill(),e.event.preventDefault()}}},_mouseUpHandler:function(){this.zr.off("mousemove",this._mouseMoveHandler),this.zr.off("mouseup",this._mouseUpHandler)},_isRightMouseButtonUsed:function(){return this.rotateMouseButton==="right"||this.panMouseButton==="right"},_contextMenuHandler:function(e){this._isRightMouseButtonUsed()&&e.preventDefault()},_addAnimator:function(e){var t=this._animators;return t.push(e),e.done(function(){var n=t.indexOf(e);n>=0&&t.splice(n,1)}),e}});Object.defineProperty(qt.prototype,"autoRotate",{get:function(e){return this._autoRotate},set:function(e){this._autoRotate=e,this._rotating=e}});function lt(){}lt.prototype={constructor:lt,setScene:function(e){this._scene=e,this._skybox&&this._skybox.attachScene(this._scene)},initLight:function(e){this._lightRoot=e,this.mainLight=new p.DirectionalLight({shadowBias:.005}),this.ambientLight=new p.AmbientLight,e.add(this.mainLight),e.add(this.ambientLight)},dispose:function(){this._lightRoot&&(this._lightRoot.remove(this.mainLight),this._lightRoot.remove(this.ambientLight))},updateLight:function(e){var t=this.mainLight,n=this.ambientLight,i=e.getModel("light"),r=i.getModel("main"),o=i.getModel("ambient");t.intensity=r.get("intensity"),n.intensity=o.get("intensity"),t.color=p.parseColor(r.get("color")).slice(0,3),n.color=p.parseColor(o.get("color")).slice(0,3);var a=r.get("alpha")||0,s=r.get("beta")||0;t.position.setArray(p.directionFromAlphaBeta(a,s)),t.lookAt(p.Vector3.ZERO),t.castShadow=r.get("shadow"),t.shadowResolution=p.getShadowResolution(r.get("shadowQuality"))},updateAmbientCubemap:function(e,t,n){var i=t.getModel("light.ambientCubemap"),r=i.get("texture");if(r){this._cubemapLightsCache=this._cubemapLightsCache||{};var o=this._cubemapLightsCache[r];if(!o){var a=this;o=this._cubemapLightsCache[r]=p.createAmbientCubemap(i.option,e,n,function(){a._isSkyboxFromAmbientCubemap&&a._skybox.setEnvironmentMap(o.specular.cubemap),n.getZr().refresh()})}this._lightRoot.add(o.diffuse),this._lightRoot.add(o.specular),this._currentCubemapLights=o}else this._currentCubemapLights&&(this._lightRoot.remove(this._currentCubemapLights.diffuse),this._lightRoot.remove(this._currentCubemapLights.specular),this._currentCubemapLights=null)},updateSkybox:function(e,t,n){var i=t.get("environment"),r=this;function o(){return r._skybox=r._skybox||new hi,r._skybox}var a=o();if(i&&i!=="none")if(i==="auto")if(this._isSkyboxFromAmbientCubemap=!0,this._currentCubemapLights){var s=this._currentCubemapLights.specular.cubemap;a.setEnvironmentMap(s),this._scene&&a.attachScene(this._scene),a.material.set("lod",3)}else this._skybox&&this._skybox.detachScene();else if(typeof i=="object"&&i.colorStops||typeof i=="string"&&Ft(i)){this._isSkyboxFromAmbientCubemap=!1;var c=new p.Texture2D({anisotropic:8,flipY:!1});a.setEnvironmentMap(c);var l=c.image=document.createElement("canvas");l.width=l.height=16;var h=l.getContext("2d"),f=new zt({shape:{x:0,y:0,width:16,height:16},style:{fill:i}});ci(h,f),a.attachScene(this._scene)}else{this._isSkyboxFromAmbientCubemap=!1;var c=p.loadTexture(i,n,{anisotropic:8,flipY:!1});a.setEnvironmentMap(c),a.attachScene(this._scene)}else this._skybox&&this._skybox.detachScene(this._scene),this._skybox=null;var d=t.coordinateSystem;if(this._skybox)if(d&&d.viewGL&&i!=="auto"&&!(i.match&&i.match(/.hdr$/))){var u=d.viewGL.isLinearSpace()?"define":"undefine";this._skybox.material[u]("fragment","SRGB_DECODE")}else this._skybox.material.undefine("fragment","SRGB_DECODE")}};var We=Dt.extend({type:"grid3D",dependencies:["xAxis3D","yAxis3D","zAxis3D"],defaultOption:{show:!0,zlevel:-10,left:0,top:0,width:"100%",height:"100%",environment:"auto",boxWidth:100,boxHeight:100,boxDepth:100,axisPointer:{show:!0,lineStyle:{color:"rgba(0, 0, 0, 0.8)",width:1},label:{show:!0,formatter:null,margin:8,textStyle:{fontSize:14,color:"#fff",backgroundColor:"rgba(0,0,0,0.5)",padding:3,borderRadius:3}}},axisLine:{show:!0,lineStyle:{color:"#333",width:2,type:"solid"}},axisTick:{show:!0,inside:!1,length:3,lineStyle:{width:1}},axisLabel:{show:!0,inside:!1,rotate:0,margin:8,textStyle:{fontSize:12}},splitLine:{show:!0,lineStyle:{color:["#ccc"],width:1,type:"solid"}},splitArea:{show:!1,areaStyle:{color:["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]}},light:{main:{alpha:30,beta:40},ambient:{intensity:.4}},viewControl:{alpha:20,beta:40,autoRotate:!1,distance:200,minDistance:40,maxDistance:400}}});ae(We.prototype,Hi);ae(We.prototype,Ui);ae(We.prototype,Gi);var de=je.vec3,Kt=H.extend(function(){return{segmentScale:1,useNativeLine:!0,attributes:{position:new H.Attribute("position","float",3,"POSITION"),normal:new H.Attribute("normal","float",3,"NORMAL"),color:new H.Attribute("color","float",4,"COLOR")}}},{resetOffset:function(){this._vertexOffset=0,this._faceOffset=0},setQuadCount:function(e){var t=this.attributes,n=this.getQuadVertexCount()*e,i=this.getQuadTriangleCount()*e;this.vertexCount!==n&&(t.position.init(n),t.normal.init(n),t.color.init(n)),this.triangleCount!==i&&(this.indices=n>65535?new Uint32Array(i*3):new Uint16Array(i*3))},getQuadVertexCount:function(){return 4},getQuadTriangleCount:function(){return 2},addQuad:(function(){var e=de.create(),t=de.create(),n=de.create(),i=[0,3,1,3,2,1];return function(r,o){var a=this.attributes.position,s=this.attributes.normal,c=this.attributes.color;de.sub(e,r[1],r[0]),de.sub(t,r[2],r[1]),de.cross(n,e,t),de.normalize(n,n);for(var l=0;l<4;l++)a.set(this._vertexOffset+l,r[l]),c.set(this._vertexOffset+l,o),s.set(this._vertexOffset+l,n);for(var h=this._faceOffset*3,l=0;l<6;l++)this.indices[h+l]=i[l]+this._vertexOffset;this._vertexOffset+=4,this._faceOffset+=2}})()});Ee(Kt.prototype,ht);var ct=J.firstNotNull,Ki={x:0,y:2,z:1};function Qi(e,t,n,i){var r=[0,0,0],o=i<0?n.getExtentMin():n.getExtentMax();r[Ki[n.dim]]=o,e.position.setArray(r),e.rotation.identity(),t.distance=-Math.abs(o),t.normal.set(0,0,0),n.dim==="x"?(e.rotation.rotateY(i*Math.PI/2),t.normal.x=-i):n.dim==="z"?(e.rotation.rotateX(-i*Math.PI/2),t.normal.y=-i):(i>0&&e.rotation.rotateY(Math.PI),t.normal.z=-i)}function Ze(e,t,n){this.rootNode=new p.Node;var i=new p.Mesh({geometry:new Xe({useNativeLine:!1}),material:t,castShadow:!1,ignorePicking:!0,$ignorePicking:!0,renderOrder:1}),r=new p.Mesh({geometry:new Kt,material:n,castShadow:!1,culling:!1,ignorePicking:!0,$ignorePicking:!0,renderOrder:0});this.rootNode.add(r),this.rootNode.add(i),this.faceInfo=e,this.plane=new p.Plane,this.linesMesh=i,this.quadsMesh=r}Ze.prototype.update=function(e,t,n){var i=e.coordinateSystem,r=[i.getAxis(this.faceInfo[0]),i.getAxis(this.faceInfo[1])],o=this.linesMesh.geometry,a=this.quadsMesh.geometry;o.convertToDynamicArray(!0),a.convertToDynamicArray(!0),this._updateSplitLines(o,r,e,n),this._udpateSplitAreas(a,r,e,n),o.convertToTypedArray(),a.convertToTypedArray();var s=i.getAxis(this.faceInfo[2]);Qi(this.rootNode,this.plane,s,this.faceInfo[3])};Ze.prototype._updateSplitLines=function(e,t,n,i){var r=i.getDevicePixelRatio();t.forEach(function(o,a){var s=o.model,c=t[1-a].getExtent();if(!o.scale.isBlank()){var l=s.getModel("splitLine",n.getModel("splitLine"));if(l.get("show")){var h=l.getModel("lineStyle"),f=h.get("color"),d=ct(h.get("opacity"),1),u=ct(h.get("width"),1);f=Ae(f)?f:[f];for(var m=o.getTicksCoords({tickModel:l}),v=0,_=0;_<m.length;_++){var g=m[_].coord,x=p.parseColor(f[v%f.length]);x[3]*=d;var b=[0,0,0],T=[0,0,0];b[a]=T[a]=g,b[1-a]=c[0],T[1-a]=c[1],e.addLine(b,T,x,u*r),v++}}}})};Ze.prototype._udpateSplitAreas=function(e,t,n,i){t.forEach(function(r,o){var a=r.model,s=t[1-o].getExtent();if(!r.scale.isBlank()){var c=a.getModel("splitArea",n.getModel("splitArea"));if(c.get("show")){var l=c.getModel("areaStyle"),h=l.get("color"),f=ct(l.get("opacity"),1);h=Ae(h)?h:[h];for(var d=r.getTicksCoords({tickModel:c,clamp:!0}),u=0,m=[0,0,0],v=[0,0,0],_=0;_<d.length;_++){var g=d[_].coord,x=[0,0,0],b=[0,0,0];if(x[o]=b[o]=g,x[1-o]=s[0],b[1-o]=s[1],_===0){m=x,v=b;continue}var T=p.parseColor(h[u%h.length]);T[3]*=f,e.addQuad([m,x,b,v],T),m=x,v=b,u++}}}})};var me=J.firstNotNull,pe={x:0,y:2,z:1};function ut(e,t){var n=new p.Mesh({geometry:new Xe({useNativeLine:!1}),material:t,castShadow:!1,ignorePicking:!0,renderOrder:2}),i=new Xt;i.material.depthMask=!1;var r=new p.Node;r.add(n),r.add(i),this.rootNode=r,this.dim=e,this.linesMesh=n,this.labelsMesh=i,this.axisLineCoords=null,this.labelElements=[]}var $e={x:"y",y:"x",z:"y"};ut.prototype.update=function(e,t,n){var i=e.coordinateSystem,r=i.getAxis(this.dim),o=this.linesMesh.geometry,a=this.labelsMesh.geometry;o.convertToDynamicArray(!0),a.convertToDynamicArray(!0);var s=r.model,c=r.getExtent(),O=n.getDevicePixelRatio(),l=s.getModel("axisLine",e.getModel("axisLine")),h=s.getModel("axisTick",e.getModel("axisTick")),f=s.getModel("axisLabel",e.getModel("axisLabel")),d=l.get("lineStyle.color");if(l.get("show")){var u=l.getModel("lineStyle"),m=[0,0,0],v=[0,0,0],_=pe[r.dim];m[_]=c[0],v[_]=c[1],this.axisLineCoords=[m,v];var g=p.parseColor(d),x=me(u.get("width"),1),b=me(u.get("opacity"),1);g[3]*=b,o.addLine(m,v,g,x*O)}if(h.get("show")){var T=h.getModel("lineStyle"),A=p.parseColor(me(T.get("color"),d)),x=me(T.get("width"),1);A[3]*=me(T.get("opacity"),1);for(var P=r.getTicksCoords(),N=h.get("length"),S=0;S<P.length;S++){var G=P[S].coord,m=[0,0,0],v=[0,0,0],_=pe[r.dim],k=pe[$e[r.dim]];m[_]=v[_]=G,v[k]=N,o.addLine(m,v,A,x*O)}}this.labelElements=[];var O=n.getDevicePixelRatio();if(f.get("show"))for(var P=r.getTicksCoords(),X=s.get("data"),W=f.get("margin"),D=r.getViewLabels(),S=0;S<D.length;S++){var I=D[S].tickValue,R=D[S].formattedLabel,z=D[S].rawLabel,G=r.dataToCoord(I),y=[0,0,0],_=pe[r.dim],k=pe[$e[r.dim]];y[_]=y[_]=G,y[k]=W;var Y=f;X&&X[I]&&X[I].textStyle&&(Y=new mn(X[I].textStyle,f,s.ecModel));var w=me(Y.get("color"),d),q=new tt({style:Je(Y,{text:R,fill:typeof w=="function"?w(r.type==="category"?z:r.type==="value"?I+"":I,S):w,verticalAlign:"top",align:"left"})}),B=t.add(q),K=q.getBoundingRect();a.addSprite(y,[K.width*O,K.height*O],B),this.labelElements.push(q)}if(s.get("name")){var Z=s.getModel("nameTextStyle"),y=[0,0,0],_=pe[r.dim],k=pe[$e[r.dim]],fe=me(Z.get("color"),d),_e=Z.get("borderColor"),x=Z.get("borderWidth");y[_]=y[_]=(c[0]+c[1])/2,y[k]=s.get("nameGap");var q=new tt({style:Je(Z,{text:s.get("name"),fill:fe,stroke:_e,lineWidth:x})}),B=t.add(q),K=q.getBoundingRect();a.addSprite(y,[K.width*O,K.height*O],B),q.__idx=this.labelElements.length,this.nameLabelElement=q}this.labelsMesh.material.set("textureAtlas",t.getTexture()),this.labelsMesh.material.set("uvScale",t.getCoordsScale()),o.convertToTypedArray(),a.convertToTypedArray()};ut.prototype.setSpriteAlign=function(e,t,n){for(var i=n.getDevicePixelRatio(),r=this.labelsMesh.geometry,o=0;o<this.labelElements.length;o++){var a=this.labelElements[o],s=a.getBoundingRect();r.setSpriteAlign(o,[s.width*i,s.height*i],e,t)}var c=this.nameLabelElement;if(c){var s=c.getBoundingRect();r.setSpriteAlign(c.__idx,[s.width*i,s.height*i],e,t),r.dirty()}this.textAlign=e,this.textVerticalAlign=t};var Et=J.firstNotNull;p.Shader.import(Fi);var ve={x:0,y:2,z:1};const $i=pn.extend({type:"grid3D",__ecgl__:!0,init:function(e,t){var n=[["y","z","x",-1,"left"],["y","z","x",1,"right"],["x","y","z",-1,"bottom"],["x","y","z",1,"top"],["x","z","y",-1,"far"],["x","z","y",1,"near"]],i=["x","y","z"],r=new p.Material({shader:p.createShader("ecgl.color"),depthMask:!1,transparent:!0}),o=new p.Material({shader:p.createShader("ecgl.meshLines3D"),depthMask:!1,transparent:!0});r.define("fragment","DOUBLE_SIDED"),r.define("both","VERTEX_COLOR"),this.groupGL=new p.Node,this._control=new qt({zr:t.getZr()}),this._control.init(),this._faces=n.map(function(s){var c=new Ze(s,o,r);return this.groupGL.add(c.rootNode),c},this),this._axes=i.map(function(s){var c=new ut(s,o);return this.groupGL.add(c.rootNode),c},this);var a=t.getDevicePixelRatio();this._axisLabelSurface=new at({width:256,height:256,devicePixelRatio:a}),this._axisLabelSurface.onupdate=function(){t.getZr().refresh()},this._axisPointerLineMesh=new p.Mesh({geometry:new Xe({useNativeLine:!1}),material:o,castShadow:!1,ignorePicking:!0,renderOrder:3}),this.groupGL.add(this._axisPointerLineMesh),this._axisPointerLabelsSurface=new at({width:128,height:128,devicePixelRatio:a}),this._axisPointerLabelsMesh=new Xt({ignorePicking:!0,renderOrder:4,castShadow:!1}),this._axisPointerLabelsMesh.material.set("textureAtlas",this._axisPointerLabelsSurface.getTexture()),this.groupGL.add(this._axisPointerLabelsMesh),this._lightRoot=new p.Node,this._sceneHelper=new lt,this._sceneHelper.initLight(this._lightRoot)},render:function(e,t,n){this._model=e,this._api=n;var i=e.coordinateSystem;i.viewGL.add(this._lightRoot),e.get("show")?i.viewGL.add(this.groupGL):i.viewGL.remove(this.groupGL);var r=this._control;r.setViewGL(i.viewGL);var o=e.getModel("viewControl");r.setFromViewControlModel(o,0),this._axisLabelSurface.clear(),r.off("update"),e.get("show")&&(this._faces.forEach(function(a){a.update(e,t,n)},this),this._axes.forEach(function(a){a.update(e,this._axisLabelSurface,n)},this)),r.on("update",this._onCameraChange.bind(this,e,n),this),this._sceneHelper.setScene(i.viewGL.scene),this._sceneHelper.updateLight(e),i.viewGL.setPostEffect(e.getModel("postEffect"),n),i.viewGL.setTemporalSuperSampling(e.getModel("temporalSuperSampling")),this._initMouseHandler(e)},afterRender:function(e,t,n,i){var r=i.renderer;this._sceneHelper.updateAmbientCubemap(r,e,n),this._sceneHelper.updateSkybox(r,e,n)},showAxisPointer:function(e,t,n,i){this._doShowAxisPointer(),this._updateAxisPointer(i.value)},hideAxisPointer:function(e,t,n,i){this._doHideAxisPointer()},_initMouseHandler:function(e){var t=e.coordinateSystem,n=t.viewGL;e.get("show")&&e.get("axisPointer.show")?n.on("mousemove",this._updateAxisPointerOnMousePosition,this):n.off("mousemove",this._updateAxisPointerOnMousePosition)},_updateAxisPointerOnMousePosition:function(e){if(!e.target){for(var t=this._model,n=t.coordinateSystem,i=n.viewGL,r=i.castRay(e.offsetX,e.offsetY,new p.Ray),o,a=0;a<this._faces.length;a++){var s=this._faces[a];if(!s.rootNode.invisible){s.plane.normal.dot(i.camera.worldTransform.z)<0&&s.plane.normal.negate();var c=r.intersectPlane(s.plane);if(c){var l=n.getAxis(s.faceInfo[0]),h=n.getAxis(s.faceInfo[1]),f=ve[s.faceInfo[0]],d=ve[s.faceInfo[1]];l.contain(c.array[f])&&h.contain(c.array[d])&&(o=c)}}}if(o){var u=n.pointToData(o.array,[],!0);this._updateAxisPointer(u),this._doShowAxisPointer()}else this._doHideAxisPointer()}},_onCameraChange:function(e,t){e.get("show")&&(this._updateFaceVisibility(),this._updateAxisLinePosition());var n=this._control;t.dispatchAction({type:"grid3DChangeCamera",alpha:n.getAlpha(),beta:n.getBeta(),distance:n.getDistance(),center:n.getCenter(),from:this.uid,grid3DId:e.id})},_updateFaceVisibility:function(){var e=this._control.getCamera(),t=new p.Vector3;e.update();for(var n=0;n<this._faces.length/2;n++){for(var i=[],r=0;r<2;r++){var o=this._faces[n*2+r];o.rootNode.getWorldPosition(t),t.transformMat4(e.viewMatrix),i[r]=t.z}var a=i[0]>i[1]?0:1,s=this._faces[n*2+a],c=this._faces[n*2+1-a];s.rootNode.invisible=!0,c.rootNode.invisible=!1}},_updateAxisLinePosition:function(){var e=this._model.coordinateSystem,t=e.getAxis("x"),n=e.getAxis("y"),i=e.getAxis("z"),r=i.getExtentMax(),o=i.getExtentMin(),a=t.getExtentMin(),s=t.getExtentMax(),c=n.getExtentMax(),l=n.getExtentMin(),h=this._axes[0].rootNode,f=this._axes[1].rootNode,d=this._axes[2].rootNode,u=this._faces,m=u[4].rootNode.invisible?l:c,v=u[2].rootNode.invisible?r:o,_=u[0].rootNode.invisible?a:s,g=u[2].rootNode.invisible?r:o,x=u[0].rootNode.invisible?s:a,b=u[4].rootNode.invisible?l:c;h.rotation.identity(),f.rotation.identity(),d.rotation.identity(),u[4].rootNode.invisible&&(this._axes[0].flipped=!0,h.rotation.rotateX(Math.PI)),u[0].rootNode.invisible&&(this._axes[1].flipped=!0,f.rotation.rotateZ(Math.PI)),u[4].rootNode.invisible&&(this._axes[2].flipped=!0,d.rotation.rotateY(Math.PI)),h.position.set(0,v,m),f.position.set(_,g,0),d.position.set(x,0,b),h.update(),f.update(),d.update(),this._updateAxisLabelAlign()},_updateAxisLabelAlign:function(){var e=this._control.getCamera(),t=[new p.Vector4,new p.Vector4],n=new p.Vector4;this.groupGL.getWorldPosition(n),n.w=1,n.transformMat4(e.viewMatrix).transformMat4(e.projectionMatrix),n.x/=n.w,n.y/=n.w,this._axes.forEach(function(i){var r=i.axisLineCoords;i.labelsMesh.geometry;for(var o=0;o<t.length;o++)t[o].setArray(r[o]),t[o].w=1,t[o].transformMat4(i.rootNode.worldTransform).transformMat4(e.viewMatrix).transformMat4(e.projectionMatrix),t[o].x/=t[o].w,t[o].y/=t[o].w;var a=t[1].x-t[0].x,s=t[1].y-t[0].y,c=(t[1].x+t[0].x)/2,l=(t[1].y+t[0].y)/2,h,f;Math.abs(s/a)<.5?(h="center",f=l>n.y?"bottom":"top"):(f="middle",h=c>n.x?"left":"right"),i.setSpriteAlign(h,f,this._api)},this)},_doShowAxisPointer:function(){this._axisPointerLineMesh.invisible&&(this._axisPointerLineMesh.invisible=!1,this._axisPointerLabelsMesh.invisible=!1,this._api.getZr().refresh())},_doHideAxisPointer:function(){this._axisPointerLineMesh.invisible||(this._axisPointerLineMesh.invisible=!0,this._axisPointerLabelsMesh.invisible=!0,this._api.getZr().refresh())},_updateAxisPointer:function(e){var t=this._model.coordinateSystem,n=t.dataToPoint(e),i=this._axisPointerLineMesh,r=i.geometry,o=this._model.getModel("axisPointer"),a=this._api.getDevicePixelRatio();r.convertToDynamicArray(!0);function s(S){return J.firstNotNull(S.model.get("axisPointer.show"),o.get("show"))}function c(S){var G=S.model.getModel("axisPointer",o),k=G.getModel("lineStyle"),O=p.parseColor(k.get("color")),X=Et(k.get("width"),1),W=Et(k.get("opacity"),1);return O[3]*=W,{color:O,lineWidth:X}}for(var l=0;l<this._faces.length;l++){var h=this._faces[l];if(!h.rootNode.invisible){for(var f=h.faceInfo,d=f[3]<0?t.getAxis(f[2]).getExtentMin():t.getAxis(f[2]).getExtentMax(),u=ve[f[2]],m=0;m<2;m++){var v=f[m],_=f[1-m],g=t.getAxis(v),x=t.getAxis(_);if(s(g)){var b=[0,0,0],T=[0,0,0],A=ve[v],P=ve[_];b[A]=T[A]=n[A],b[u]=T[u]=d,b[P]=x.getExtentMin(),T[P]=x.getExtentMax();var N=c(g);r.addLine(b,T,N.color,N.lineWidth*a)}}if(s(t.getAxis(f[2]))){var b=n.slice(),T=n.slice();T[u]=d;var N=c(t.getAxis(f[2]));r.addLine(b,T,N.color,N.lineWidth*a)}}}r.convertToTypedArray(),this._updateAxisPointerLabelsMesh(e),this._api.getZr().refresh()},_updateAxisPointerLabelsMesh:function(e){var t=this._model,n=this._axisPointerLabelsMesh,i=this._axisPointerLabelsSurface,r=t.coordinateSystem,o=t.getModel("axisPointer");n.geometry.convertToDynamicArray(!0),i.clear();var a={x:"y",y:"x",z:"y"};this._axes.forEach(function(s,c){var l=r.getAxis(s.dim),h=l.model,f=h.getModel("axisPointer",o),d=f.getModel("label"),u=f.get("lineStyle.color");if(!(!d.get("show")||!f.get("show"))){var m=e[c],v=d.get("formatter"),_=l.scale.getLabel({value:m});if(v!=null)_=v(_,e);else if(l.scale.type==="interval"||l.scale.type==="log"){var g=Nt(l.scale.getTicks()[0]);_=m.toFixed(g+2)}var x=d.get("color"),b=new tt({style:Je(d,{text:_,fill:x||u,align:"left",verticalAlign:"top"})}),T=i.add(b),A=b.getBoundingRect(),P=this._api.getDevicePixelRatio(),N=s.rootNode.position.toArray(),S=ve[a[s.dim]];N[S]+=(s.flipped?-1:1)*d.get("margin"),N[ve[s.dim]]=l.dataToCoord(e[c]),n.geometry.addSprite(N,[A.width*P,A.height*P],T,s.textAlign,s.textVerticalAlign)}},this),i.getZr().refreshImmediately(),n.material.set("uvScale",i.getCoordsScale()),n.geometry.convertToTypedArray()},dispose:function(){this.groupGL.removeAll(),this._control.dispose(),this._axisLabelSurface.dispose(),this._axisPointerLabelsSurface.dispose()}});function Le(e){Mt.call(this,e),this.type="cartesian3D",this.dimensions=["x","y","z"],this.size=[0,0,0]}Le.prototype={constructor:Le,model:null,containPoint:function(e){return this.getAxis("x").contain(e[0])&&this.getAxis("y").contain(e[2])&&this.getAxis("z").contain(e[1])},containData:function(e){return this.getAxis("x").containData(e[0])&&this.getAxis("y").containData(e[1])&&this.getAxis("z").containData(e[2])},dataToPoint:function(e,t,n){return t=t||[],t[0]=this.getAxis("x").dataToCoord(e[0],n),t[2]=this.getAxis("y").dataToCoord(e[1],n),t[1]=this.getAxis("z").dataToCoord(e[2],n),t},pointToData:function(e,t,n){return t=t||[],t[0]=this.getAxis("x").coordToData(e[0],n),t[1]=this.getAxis("y").coordToData(e[2],n),t[2]=this.getAxis("z").coordToData(e[1],n),t}};Ht(Le,Mt);function ke(e,t,n){Rt.call(this,e,t,n)}ke.prototype={constructor:ke,getExtentMin:function(){var e=this._extent;return Math.min(e[0],e[1])},getExtentMax:function(){var e=this._extent;return Math.max(e[0],e[1])},calculateCategoryInterval:function(){return Math.floor(this.scale.count()/8)}};Ht(ke,Rt);function Ji(e,t){var n=e.getBoxLayoutParams(),i=vn(n,{width:t.getWidth(),height:t.getHeight()});i.y=t.getHeight()-i.y-i.height,this.viewGL.setViewport(i.x,i.y,i.width,i.height,t.getDevicePixelRatio());var r=e.get("boxWidth"),o=e.get("boxHeight"),a=e.get("boxDepth");this.getAxis("x").setExtent(-r/2,r/2),this.getAxis("y").setExtent(a/2,-a/2),this.getAxis("z").setExtent(-o/2,o/2),this.size=[r,o,a]}function er(e,t){var n={};function i(r,o){n[r]=n[r]||[1/0,-1/0],n[r][0]=Math.min(o[0],n[r][0]),n[r][1]=Math.max(o[1],n[r][1])}e.eachSeries(function(r){if(r.coordinateSystem===this){var o=r.getData();["x","y","z"].forEach(function(a){o.mapDimensionsAll(a,!0).forEach(function(s){i(a,o.getDataExtent(s,!0))})})}},this),["xAxis3D","yAxis3D","zAxis3D"].forEach(function(r){e.eachComponent(r,function(o){var a=r.charAt(0),s=o.getReferringComponents("grid3D").models[0],c=s.coordinateSystem;if(c===this){var l=c.getAxis(a);if(!l){var h=_n(n[a]||[1/0,-1/0],o);l=new ke(a,h),l.type=o.get("type");var f=l.type==="category";l.onBand=f&&o.get("boundaryGap"),l.inverse=o.get("inverse"),o.axis=l,l.model=o,l.getLabelModel=function(){return o.getModel("axisLabel",s.getModel("axisLabel"))},l.getTickModel=function(){return o.getModel("axisTick",s.getModel("axisTick"))},c.addAxis(l)}}},this)},this),this.resize(this.model,t)}var tr={dimensions:Le.prototype.dimensions,create:function(e,t){var n=[];e.eachComponent("grid3D",function(o){o.__viewGL=o.__viewGL||new M;var a=new Le;a.model=o,a.viewGL=o.__viewGL,o.coordinateSystem=a,n.push(a),a.resize=Ji,a.update=er});var i=["xAxis3D","yAxis3D","zAxis3D"];function r(o,a){return i.map(function(s){var c=o.getReferringComponents(s).models[0];return c==null&&(c=a.getComponent(s)),c})}return e.eachSeries(function(o){if(o.get("coordinateSystem")==="cartesian3D"){var a=o.getReferringComponents("grid3D").models[0];if(a==null){var s=r(o,e),a=s[0].getCoordSysModel();s.forEach(function(h){h.getCoordSysModel()})}var c=a.coordinateSystem;o.coordinateSystem=c}}),n}},Qt=Dt.extend({type:"cartesian3DAxis",axis:null,getCoordSysModel:function(){return this.ecModel.queryComponents({mainType:"grid3D",index:this.option.gridIndex,id:this.option.gridId})[0]}});gn(Qt);var $t={show:!0,grid3DIndex:0,inverse:!1,name:"",nameLocation:"middle",nameTextStyle:{fontSize:16},nameGap:20,axisPointer:{},axisLine:{},axisTick:{},axisLabel:{},splitArea:{}},nr=ae({boundaryGap:!0,axisTick:{alignWithLabel:!1,interval:"auto"},axisLabel:{interval:"auto"},axisPointer:{label:{show:!1}}},$t),dt=ae({boundaryGap:[0,0],splitNumber:5,axisPointer:{label:{}}},$t),ir=Ee({scale:!0,min:"dataMin",max:"dataMax"},dt),Jt=Ee({logBase:10},dt);Jt.scale=!0;const rr={categoryAxis3D:nr,valueAxis3D:dt,timeAxis3D:ir,logAxis3D:Jt};var or=["value","category","time","log"];function ar(e,t,n,i,r){or.forEach(function(o){var a=n.extend({type:t+"Axis3D."+o,__ordinalMeta:null,mergeDefaultAndTheme:function(s,c){var l=c.getTheme();ae(s,l.get(o+"Axis3D")),ae(s,this.getDefaultOption()),s.type=i(t,s)},optionUpdated:function(){var s=this.option;s.type==="category"&&(this.__ordinalMeta=xn.createByAxisModel(this))},getCategories:function(){if(this.option.type==="category")return this.__ordinalMeta.categories},getOrdinalMeta:function(){return this.__ordinalMeta},defaultOption:ae(fi(rr[o+"Axis3D"]),r||{},!0)});e.registerComponentModel(a)}),e.registerSubTypeDefaulter(t+"Axis3D",ui(i,t))}function sr(e,t){return t.type||(t.data?"category":"value")}function fr(e){e.registerComponentModel(We),e.registerComponentView($i),e.registerCoordinateSystem("grid3D",tr),["x","y","z"].forEach(function(t){ar(e,t,Qt,sr,{name:t.toUpperCase()});const n=e.ComponentView.extend({type:t+"Axis3D"});e.registerComponentView(n)}),e.registerAction({type:"grid3DChangeCamera",event:"grid3dcamerachanged",update:"series:updateCamera"},function(t,n){n.eachComponent({mainType:"grid3D",query:t},function(i){i.setView(t)})}),e.registerAction({type:"grid3DShowAxisPointer",event:"grid3dshowaxispointer",update:"grid3D:showAxisPointer"},function(t,n){}),e.registerAction({type:"grid3DHideAxisPointer",event:"grid3dhideaxispointer",update:"grid3D:hideAxisPointer"},function(t,n){})}export{fr as a,hr as i};
