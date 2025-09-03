#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in vec2 g_tex_coord[];
in float g_width[];
in float g_height[];
in vec4 g_color[];

uniform float hori_res;
uniform float vert_res;
uniform float tex_width;
uniform float tex_height;

out vec2 f_tex_coord;
out vec4 f_color;

void main() {
  float vw = g_width[0] * 2 / hori_res;
  float vh = g_height[0] * 2 / vert_res;
  float tw = g_width[0] / tex_width;
  float th = g_height[0] / tex_height;
  vec4 pos = gl_in[0].gl_Position;
  vec2 tpos = g_tex_coord[0].xy / vec2(tex_width, tex_height);

  gl_Position = pos;
  f_tex_coord = tpos;
  f_color = g_color[0];
  EmitVertex();

  gl_Position = pos + vec4(vw, 0, 0, 0);
  f_tex_coord = tpos + vec2(tw, 0);
  f_color = g_color[0];
  EmitVertex();

  gl_Position = pos + vec4(0, vh, 0, 0);
  f_tex_coord = tpos + vec2(0, th);
  f_color = g_color[0];
  EmitVertex();

  gl_Position = pos + vec4(vw, vh, 0, 0);
  f_tex_coord = tpos + vec2(tw, th);
  f_color = g_color[0];
  EmitVertex();

  EndPrimitive();
}
