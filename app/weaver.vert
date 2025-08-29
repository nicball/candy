#version 330 core

layout (location = 0) in vec2 v_pos;
layout (location = 1) in float v_width;
layout (location = 2) in float v_height;
layout (location = 3) in vec2 v_tex_coord;

out vec2 g_tex_coord;
out float g_width;
out float g_height;

uniform float hori_res;
uniform float vert_res;

void main() {
  gl_Position = vec4(v_pos.x * 2 / hori_res - 1, -(v_pos.y + 1) * 2 / vert_res + 1, 0, 1);
  g_tex_coord = v_tex_coord;
  g_width = v_width;
  g_height = v_height;
}
