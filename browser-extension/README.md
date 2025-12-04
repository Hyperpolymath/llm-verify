# Claude Verify Browser Extension

Browser extension to inject project context into Claude.ai and verify LLM-generated code.

## Features

- Load `.claude-context.toml` from your project
- Inject known issues into Claude conversations
- Add "Verify" buttons to code blocks
- Record feedback when Claude makes mistakes
- Connect to local verification daemon

## Installation

### Chrome / Edge / Brave

1. Go to `chrome://extensions`
2. Enable "Developer mode"
3. Click "Load unpacked"
4. Select this `browser-extension` directory

### Firefox

1. Go to `about:debugging`
2. Click "This Firefox"
3. Click "Load Temporary Add-on"
4. Select `manifest.json` from this directory

## Usage

### Loading Project Context

1. Click the Claude Verify icon in your browser toolbar
2. Click "Load .claude-context.toml"
3. Select your project's context file

### Injecting Context

1. With a project loaded, click "Inject into Chat"
2. The known issues will be added to your conversation

### Verifying Code

1. Start the verification daemon: `claude-verify daemon`
2. Hover over any code block on Claude.ai
3. Click the "Verify" button
4. Results appear as indicators on the code block

### Recording Feedback

When Claude generates incorrect code:

1. Click "Wrong?" on the code block
2. Describe what was wrong
3. Optionally provide the correction
4. Submit

Feedback is stored locally and can be exported.

## Daemon

The extension connects to a local daemon for verification:

```bash
# Start the daemon
claude-verify daemon --port 9847

# Or with verbose logging
claude-verify daemon --verbose
```

The daemon provides:
- `/api/health` - Health check
- `/api/verify` - Code verification
- `/api/feedback` - Feedback submission

## Icon Generation

To generate PNG icons from the SVG:

```bash
# Requires ImageMagick
for size in 16 32 48 128; do
  convert -background none icons/icon.svg -resize ${size}x${size} icons/icon-${size}.png
done
```

## Privacy

- All data stored locally in browser storage
- Feedback only sent to local daemon (never to cloud)
- No analytics or tracking

## License

MIT
