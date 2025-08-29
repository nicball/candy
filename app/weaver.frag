#version 330 core

in vec2 f_tex_coord;

uniform sampler2D atlas;
uniform vec4 pen_color;

out vec4 color;

void main() {
  color = vec4(pen_color.rgb, texture(atlas, f_tex_coord).r * pen_color.a);
}
