#include "SDL2/SDL.h"
#include "SDL2/SDL_ttf.h"

extern DECLSPEC SDL_Surface * SDLCALL
  TTF_RenderUNICODE_Blended_(TTF_Font *font, uint16_t *text, SDL_Color *fg)
{
  return TTF_RenderUNICODE_Blended(font, text, *fg);
}
