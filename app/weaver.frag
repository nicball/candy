#version 330 core

in vec2 f_tex_coord;

uniform sampler2D atlas;
uniform vec4 pen_color;

out vec4 color;

uniform float tex_width;
uniform float tex_height;

void main() {
  float font_grey = texture(atlas, f_tex_coord).r;
  color = vec4(pen_color.rgb, font_grey * pen_color.a);
  // vec2 uv = f_tex_coord * vec2(tex_width, tex_height) - vec2(0.5, 0.5);
  // float y = mod(uv.y, 69);
  // if (abs(y) < 0.2)
  //   color = vec4(1, 0, 0, 1);
  // else if (abs(y - 1) < 0.2)
  //   color = vec4(0, 1, 0, 1);
  // else if (abs(y - 2) < 0.2)
  //   color = vec4(0, 0, 1, 1);
  // else
  //   color = vec4(0, 0, 0, 1);
}
