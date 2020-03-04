module Shader exposing (fragmentShader, mesh, vertexShader)

import Math.Vector2 as Vector2
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL


type alias Vertex =
    { position : Vec3
    }


mesh : WebGL.Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 -1 -1 0)
          )
        , ( Vertex (vec3 -1 -1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 1 -1 0)
          )
        ]


type alias Uniforms =
    { time : Float }


vertexShader : WebGL.Shader Vertex Uniforms { vFragCoord : Vector2.Vec2 }
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        varying vec2 vFragCoord;

        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = position.xy;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vFragCoord : Vector2.Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 vFragCoord;
        uniform float time;

        void main () {
            vec2 uv = vFragCoord;
            gl_FragColor = vec4(vec3(1.0, 0.0 + sin(time * 0.1), 0.0 + uv.x) * 0.1, 0.0);
        }
    |]
