// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

//  Copyright(c) 2016, Michal Skalsky
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without modification,
//  are permitted provided that the following conditions are met:
//
//  1. Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//  3. Neither the name of the copyright holder nor the names of its contributors
//     may be used to endorse or promote products derived from this software without
//     specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.IN NO EVENT
//  SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
//  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
//  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
//  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



Shader "Hidden/AtmosphericScattering"
{
	Properties
	{
		_MainTex ("Texture", 2D) = "white" {}
		_ZTest ("ZTest", Float) = 0
	}
	SubShader
	{
		Tags { "RenderType"="Opaque" }
		LOD 100

		CGINCLUDE
		#include "UnityCG.cginc"
		#include "UnityDeferredLibrary.cginc"

		#include "AtmosphericScattering.cginc"

		sampler2D _LightShaft1;

		struct appdata
		{
			float4 vertex : POSITION;
		};
		
		float _DistanceScale;

		struct v2f
		{
			float4 pos : SV_POSITION;
			float4 uv : TEXCOORD0;
			float3 wpos : TEXCOORD1;
		};
		               
		ENDCG
            
		// pass 0 - precompute particle density
		Pass
		{
			ZTest Off
			Cull Off
			ZWrite Off
			Blend Off

			CGPROGRAM

            #pragma vertex vertQuad
            #pragma fragment particleDensityLUT
            #pragma target 4.0

            #define UNITY_HDR_ON

            struct v2p
            {
                float4 pos : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            struct input
            {
                float4 vertex : POSITION;
                float2 texcoord : TEXCOORD0;
            };

            v2p vertQuad(input v)
            {
                v2p o;
                o.pos = UnityObjectToClipPos(v.vertex);
                o.uv = v.texcoord.xy;
                return o;
            }

			float2 particleDensityLUT(v2p i) : SV_Target
			{
                float cosAngle = i.uv.x * 2.0 - 1.0;
                float sinAngle = sqrt(saturate(1 - cosAngle * cosAngle));
                float startHeight = lerp(0.0, _AtmosphereHeight, i.uv.y);

                float3 rayStart = float3(0, startHeight, 0);
                float3 rayDir = float3(sinAngle, cosAngle, 0);
                
				return PrecomputeParticleDensity(rayStart, rayDir);
			}

			ENDCG
		}
			
		// pass 1 - ambient light LUT
		Pass
		{
			ZTest Off
			Cull Off
			ZWrite Off
			Blend Off

			CGPROGRAM

#pragma vertex vertQuad
#pragma fragment fragDir
#pragma target 4.0

			struct v2p
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
			};

			struct input
			{
				float4 vertex : POSITION;
				float2 texcoord : TEXCOORD0;
			};

			v2p vertQuad(input v)
			{
				v2p o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uv = v.texcoord.xy;
				return o;
			}

			float4 fragDir(v2p i) : SV_Target
			{
				float cosAngle = i.uv.x * 1.1 - 0.1;// *2.0 - 1.0;
                float sinAngle = sqrt(saturate(1 - cosAngle * cosAngle));
                    
                float3 lightDir = -normalize(float3(sinAngle, cosAngle, 0));

				return PrecomputeAmbientLight(lightDir);
			}
			ENDCG
		}

		// pass 2 - dir light LUT
		Pass
		{
			ZTest Off
			Cull Off
			ZWrite Off
			Blend Off

			CGPROGRAM

#pragma vertex vertQuad
#pragma fragment fragDir
#pragma target 4.0

			struct v2p
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
			};

			struct input
			{
				float4 vertex : POSITION;
				float2 texcoord : TEXCOORD0;
			};

			v2p vertQuad(input v)
			{
				v2p o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uv = v.texcoord.xy;
				return o;
			}

			float4 fragDir(v2p i) : SV_Target
			{
				float cosAngle = i.uv.x * 1.1 - 0.1;// *2.0 - 1.0;
				
				float sinAngle = sqrt(saturate(1 - cosAngle * cosAngle));				
				float3 rayDir = normalize(float3(sinAngle, cosAngle, 0));

				return PrecomputeDirLight(rayDir);
			}
			ENDCG
		}

			
		// pass 3 - atmocpheric fog
		Pass
		{
			ZTest Always Cull Off ZWrite Off
			Blend One Zero

			CGPROGRAM

#pragma vertex vertDir
#pragma fragment fragDir
#pragma target 4.0

#define UNITY_HDR_ON

#pragma shader_feature ATMOSPHERE_REFERENCE
#pragma shader_feature LIGHT_SHAFTS
#pragma shader_feature SKYBOX_BLEND

			sampler2D _Background;

			float4 _SkyboxBlend;
			float4 _HeightParams;
			uniform float _FogDensity = 0.005;
			uniform float _FogHeight = 1000;

			float _FogReferenceHeightOffset;
			float _FogColorMultiplier;

			struct VSInput
			{
				float4 vertex : POSITION;
				float2 uv : TEXCOORD0;
				uint vertexId : SV_VertexID;
			};

			struct PSInput
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
				float3 wpos : TEXCOORD1;
			};

			PSInput vertDir(VSInput i)
			{
				PSInput o;

				o.pos = UnityObjectToClipPos(i.vertex);
				o.uv = i.uv;
				o.wpos = _FrustumCorners[i.vertexId];

				return o;
			}

			// Linear half-space fog, from https://www.terathon.com/lengyel/Lengyel-UnifiedFog.pdf
			float ComputeHalfSpace(float3 wsDir, float3 wsCamera, float4 _HeightParams)
			{
				float3 wpos = wsCamera + wsDir;
				float FH = _HeightParams.x;
				float3 C = wsCamera;
				float3 V = wsDir;
				float3 P = wpos;
				float3 aV = _HeightParams.w * V;
				float FdotC = _HeightParams.y;
				float k = _HeightParams.z;
				float FdotP = P.y - FH;
				float FdotV = wsDir.y;
				float c1 = k * (FdotP + FdotC);
				float c2 = (1 - 2 * k) * FdotP;
				float g = min(c2, 0.0);
				g = -length(aV) * (c1 - g * g / abs(FdotV + 1.0e-5f));
				return g;
			}

			float4 fragDir(PSInput i) : COLOR0
			{
				float2 uv = i.uv.xy;
				float depth = SAMPLE_DEPTH_TEXTURE(_CameraDepthTexture, uv);
				float linearDepth = Linear01Depth(depth);

				float3 wpos = i.wpos;
				float3 rayStart = _WorldSpaceCameraPos;
				rayStart.y += _FogReferenceHeightOffset;

				float3 rayDir = wpos - _WorldSpaceCameraPos;
				rayDir *= linearDepth;

				float rayLength = length(rayDir);
				rayDir /= rayLength;
					
				float3 planetCenter = _WorldSpaceCameraPos;
				planetCenter = float3(0, -_PlanetRadius, 0);
				float2 intersection = RaySphereIntersection(rayStart, rayDir, planetCenter, _PlanetRadius + _AtmosphereHeight);
				if (linearDepth > 0.99999)
				{
					rayLength = 1e20;
				}
				rayLength = min(intersection.y, rayLength);

				intersection = RaySphereIntersection(rayStart, rayDir, planetCenter, _PlanetRadius);
				if (intersection.x > 0)
					rayLength = min(rayLength, intersection.x);

				float4 extinction;
				_SunIntensity = 0;
				float4 inscattering = IntegrateInscattering(rayStart, rayDir, rayLength, planetCenter, _DistanceScale, _LightDir, 16, extinction);
					
				//return inscattering;

#ifndef ATMOSPHERE_REFERENCE
				inscattering.xyz = tex3D(_InscatteringLUT, float3(uv.x, uv.y, linearDepth));
				extinction.xyz = tex3D(_ExtinctionLUT, float3(uv.x, uv.y, linearDepth));
#endif					
#ifdef LIGHT_SHAFTS
				float shadow = tex2D(_LightShaft1, uv.xy).x;
				shadow = (pow(shadow, 4) + shadow) / 2;
				shadow = max(0.1, shadow);

				inscattering *= shadow;
#endif
				float4 background = tex2D(_Background, uv);

				if (linearDepth > 0.99999)
				{
#ifdef LIGHT_SHAFTS
					background *= shadow;
#endif
					inscattering = 0;
					extinction = 1;
				}

				float4 c = background * extinction + inscattering;

#ifdef SKYBOX_BLEND
				// SKYBOX - copied from skybox
				float3 lightDir = -_WorldSpaceLightPos0.xyz;

				float4 scatterR = 0;
				float4 scatterM = 0;

				float height = length(rayStart - planetCenter) - _PlanetRadius;
				float3 normal = normalize(rayStart - planetCenter);

				// Nothke: mirror ground
				float3 rayDirMirror = rayDir;
				//rayDirMirror.y = abs(rayDirMirror.y);
				rayDirMirror.y = max(0, rayDirMirror.y);
				rayDirMirror.y = 0;

				float viewZenith = dot(normal, rayDirMirror);
				float sunZenith = dot(normal, -lightDir);

				float3 coords = float3(height / _AtmosphereHeight, viewZenith * 0.5 + 0.5, sunZenith * 0.5 + 0.5);

				coords.x = pow(height / _AtmosphereHeight, 0.5);
				float ch = -(sqrt(height * (2 * _PlanetRadius + height)) / (_PlanetRadius + height));
				if (viewZenith > ch)
				{
					coords.y = 0.5 * pow((viewZenith - ch) / (1 - ch), 0.2) + 0.5;
				}
				else
				{
					coords.y = 0.5 * pow((ch - viewZenith) / (ch + 1), 0.2);
				}
				coords.z = 0.5 * ((atan(max(sunZenith, -0.1975) * tan(1.26*1.1)) / 1.1) + (1 - 0.26));

				scatterR = tex3D(_SkyboxLUT, coords);

				// HIGH_QUALITY
				//scatterM.x = scatterR.w;
				//scatterM.yz = tex3D(_SkyboxLUT2, coords).xy;
				// !HIGH_QUALITY
				scatterM.xyz = scatterR.xyz * ((scatterR.w) / (scatterR.x));

				float3 m = scatterM;
				//scatterR = 0;
				// phase function


				ApplyPhaseFunctionElek(scatterR.xyz, scatterM.xyz, dot(rayDirMirror, -lightDir.xyz));
				float3 lightInscatter = (scatterR * _ScatteringR + scatterM * _ScatteringM) * _IncomingLight.xyz;
//#ifdef RENDER_SUN
				//lightInscatter += RenderSun(m, dot(rayDir, -lightDir.xyz)) * 10;
//#endif
				//return background;
				float4 skyboxScatter = float4(max(0, lightInscatter), 1);

				// Colorize blend
				//skyboxScatter *= float4(0.8, 0.8, 0, 1);
				skyboxScatter *= _FogColorMultiplier;

				// END SKYBOX

				//background = lerp(background, skyboxScatter, distanceFactor);
				// inverse lerp: (T - A) / (B - A);
				float ivDistanceFactor = (rayLength - _SkyboxBlend.x) / (_SkyboxBlend.y - _SkyboxBlend.x);
				//ivDistanceFactor = saturate(ivDistanceFactor);
				ivDistanceFactor = smoothstep(0, 1, ivDistanceFactor);

				// HEIGHT FOG

				float g = 0;

				// heightfog multiplies with far plane
				// Warning: clamped to prevent precision errors
				float rayLengthClamped = clamp(rayLength, 0, 1000000);
				float3 fogRay = rayDir * rayLengthClamped;
				//return saturate(rayLength * 0.01);

				_HeightParams.x = _FogHeight;
				_HeightParams.y = _WorldSpaceCameraPos.y - _HeightParams.x; // temp
				_HeightParams.z = (_HeightParams.y <= 0.0f ? 1.0f : 0.0f); // temp
				_HeightParams.w = _FogDensity * 0.5;

				g += ComputeHalfSpace(fogRay, _WorldSpaceCameraPos, _HeightParams);
				_FogDensity *= 1.2011224087f;
				float fogFac = _FogDensity * max(0.0, g);
				fogFac = exp2(-fogFac * fogFac);
				fogFac = saturate(fogFac);

				//skyboxScatter = lerp(skyboxScatter, 0, fogFac);

				// Combine fog factor and blend distance factor
				float finalScatterFac = (1-fogFac)* ivDistanceFactor;

#ifdef LIGHT_SHAFTS
				finalScatterFac *= lerp(1, 1, shadow);
#endif

				// lerp skybox blend
				c = lerp(c, skyboxScatter, finalScatterFac);
				//return c;

				//c = lerp(c, skyboxScatter, ivDistanceFactor);
				if (linearDepth > 0.99999) // is skybox
					return c;
				else
					return lerp(c, skyboxScatter, saturate(linearDepth * 1.5));
#endif

				return c;
			}
			ENDCG
		}
	}
}
