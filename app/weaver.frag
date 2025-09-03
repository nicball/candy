#version 330 core

in vec2 f_tex_coord;
in vec4 f_color;

uniform sampler2D atlas;

out vec4 color;

void main() {
  float font_grey = texture(atlas, f_tex_coord).r;
  color = vec4(f_color.rgb, font_grey * f_color.a);
}
