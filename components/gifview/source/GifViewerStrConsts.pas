Unit GifViewerStrConsts;

{$mode objfpc}{$H+}

Interface

ResourceString
  // Messages d'erreurs ou de notifications
  // Error or notification messages
  //uGifViewer
  rsScreenBadColorSize       = 'Invalid number of colors in the global palette.';
  rsImageBadColorSize        = 'Number of colors is invalid in local palette.';
  rsBadSignature             = 'GIF invalid signature: %s';
  rsBadScreenSize            = 'Invalid image size: %dx%d';
  rsEmptyColorMap            = 'Error no palette of color available for this image!';
  rsEmptyImage               = 'The picture is empty';
  rsUnknownVersion           = 'Unknown GIF version';
  rsFileNotFound             = 'The file %s  not found !';
  rsResourceNotFound         = 'Resource %s not found!';
  rsBufferOverFlow           = 'Image #%d: The decoder has been stopped to prevent a buffer overflow';
  rsInvalidOutputBufferSize  = 'Image #%d: The size of the output buffer is invalid (size < = 0)';
  rsInvalidInputBufferSize   = 'Image #%d: The size of the input buffer is invalid (size < = 0)';
  rsInvalidBufferSize        = 'Image #%d: The size of the input and output buffer are invalid (size < = 0)';
  rsLZWInternalErrorOutputBufferOverflow = 'Output buffer overflow in the GIF LZW decoder buffer. Report this bug. This is a serious bug!';
  rsLZWInternalErrorInputBufferOverflow  = 'Input buffer overflow in the GIF LZW decoder. Report this bug. This is a serious bug!';
  rsLZWInvalidInput         = 'Image #%d: The decoder encountered an invalid entry (corrupted data)';
  rsLZWOutputBufferTooSmall  = 'Image #%d: The decoder could not decode all the data because the output buffer is too small';
  rsAllFrameCorrupted        = 'All images in the GIF are corrupted. Unable to display GIF file.';
  //uFastBitmap
  rsBitmapCreateError = 'An Error occured while creating TBitmap';
  rsBitmapScanlineOutOfRange = 'Scanline : Index Out of range';

Implementation

End.

