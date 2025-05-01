#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in vec2 g_tex_coord[];
in float g_width[];
in float g_height[];

uniform float hori_ppu;
uniform float vert_ppu;
uniform float tex_width;
uniform float tex_height;

out vec2 f_tex_coord;

void main() {
  float vw = g_width[0] / hori_ppu;
  float vh = g_height[0] / vert_ppu;
  float tw = g_width[0] / tex_width;
  float th = g_height[0] / tex_height;
  vec4 pos = gl_in[0].gl_Position;
  vec2 tpos = vec2(g_tex_coord[0].x / tex_width, g_tex_coord[0].y / tex_height);

  gl_Position = pos;
  f_tex_coord = tpos;
  EmitVertex();

  gl_Position = pos + vec4(vw, 0, 0, 0);
  f_tex_coord = tpos + vec2(tw, 0);
  EmitVertex();

  gl_Position = pos + vec4(0, vh, 0, 0);
  f_tex_coord = tpos + vec2(0, th);
  EmitVertex();

  gl_Position = pos + vec4(vw, vh, 0, 0);
  f_tex_coord = tpos + vec2(tw, th);
  EmitVertex();

  EndPrimitive();
}
