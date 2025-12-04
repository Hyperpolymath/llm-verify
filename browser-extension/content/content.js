// Claude Verify - Content Script for Claude.ai

// State
let projectContext = null;
let knownIssues = [];

// Initialize
console.log('[Claude Verify] Content script loaded');

// Listen for messages from popup
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  switch (message.type) {
    case 'PROJECT_LOADED':
      projectContext = message.project;
      knownIssues = message.knownIssues;
      showNotification('Project loaded: ' + projectContext.name);
      addVerifyButtons();
      break;

    case 'INJECT_CONTEXT':
      injectContextIntoChat(message.context);
      break;

    case 'GET_SELECTED_CODE':
      const code = getSelectedCode();
      sendResponse(code);
      break;

    case 'SHOW_VERIFICATION_RESULT':
      showVerificationResult(message.result);
      break;
  }
  return true;
});

// Load saved project on page load
chrome.storage.local.get(['project', 'knownIssues'], (stored) => {
  if (stored.project) {
    projectContext = stored.project;
    knownIssues = stored.knownIssues || [];
    addVerifyButtons();
  }
});

// Inject context into the chat input
function injectContextIntoChat(context) {
  // Find Claude's chat input
  const textarea = document.querySelector('div[contenteditable="true"]');
  if (!textarea) {
    console.error('[Claude Verify] Could not find chat input');
    showNotification('Could not find chat input', 'error');
    return;
  }

  // Insert the context as a new message
  textarea.focus();

  // Create a paste event with the context
  const pasteEvent = new ClipboardEvent('paste', {
    bubbles: true,
    cancelable: true,
    clipboardData: new DataTransfer()
  });

  pasteEvent.clipboardData.setData('text/plain', context);
  textarea.dispatchEvent(pasteEvent);

  // If paste didn't work, try direct insertion
  if (!textarea.textContent.includes(context.substring(0, 20))) {
    textarea.textContent = context;
    textarea.dispatchEvent(new Event('input', { bubbles: true }));
  }

  showNotification('Context injected into chat');
}

// Get selected code from the page
function getSelectedCode() {
  // Check for selected text first
  const selection = window.getSelection();
  if (selection && selection.toString().trim()) {
    return {
      code: selection.toString(),
      language: detectLanguage(selection.toString())
    };
  }

  // Check for code blocks with active selection indicator
  const activeCodeBlock = document.querySelector('.code-block.selected, pre.selected');
  if (activeCodeBlock) {
    return {
      code: activeCodeBlock.textContent,
      language: activeCodeBlock.dataset?.language || detectLanguageFromElement(activeCodeBlock)
    };
  }

  // Get the last code block from Claude's response
  const codeBlocks = document.querySelectorAll('pre code, .code-block');
  if (codeBlocks.length > 0) {
    const lastBlock = codeBlocks[codeBlocks.length - 1];
    return {
      code: lastBlock.textContent,
      language: lastBlock.className?.match(/language-(\w+)/)?.[1] || 'unknown'
    };
  }

  return null;
}

// Detect language from code content
function detectLanguage(code) {
  if (code.includes('fn ') && code.includes('let ') && code.includes('->')) return 'rust';
  if (code.includes('def ') && code.includes(':') && !code.includes('{')) return 'python';
  if (code.includes('function') || code.includes('=>') || code.includes('const ')) return 'javascript';
  if (code.includes('func ') && code.includes(':=')) return 'go';
  if (code.includes('procedure') || code.includes('begin') || code.includes('end;')) return 'ada';
  if (code.includes('where') && code.includes('::') && code.includes('->')) return 'haskell';
  return 'unknown';
}

// Detect language from element classes
function detectLanguageFromElement(element) {
  const classes = element.className || '';
  const match = classes.match(/language-(\w+)|lang-(\w+)|(\w+)-code/);
  if (match) return match[1] || match[2] || match[3];
  return 'unknown';
}

// Add verify buttons to code blocks
function addVerifyButtons() {
  // Wait for DOM to settle
  setTimeout(() => {
    const codeBlocks = document.querySelectorAll('pre:not([data-cv-processed])');

    codeBlocks.forEach(block => {
      block.dataset.cvProcessed = 'true';

      // Create button container
      const buttonContainer = document.createElement('div');
      buttonContainer.className = 'cv-button-container';

      // Verify button
      const verifyBtn = document.createElement('button');
      verifyBtn.className = 'cv-verify-btn';
      verifyBtn.textContent = 'Verify';
      verifyBtn.title = 'Verify this code with ECHIDNA';
      verifyBtn.onclick = (e) => {
        e.preventDefault();
        e.stopPropagation();
        verifyCodeBlock(block);
      };

      // Feedback button
      const feedbackBtn = document.createElement('button');
      feedbackBtn.className = 'cv-feedback-btn';
      feedbackBtn.textContent = 'Wrong?';
      feedbackBtn.title = 'Report an issue with this code';
      feedbackBtn.onclick = (e) => {
        e.preventDefault();
        e.stopPropagation();
        reportCodeIssue(block);
      };

      buttonContainer.appendChild(verifyBtn);
      buttonContainer.appendChild(feedbackBtn);

      // Position the container
      block.style.position = 'relative';
      block.appendChild(buttonContainer);
    });
  }, 500);

  // Watch for new code blocks
  const observer = new MutationObserver((mutations) => {
    for (const mutation of mutations) {
      if (mutation.addedNodes.length) {
        addVerifyButtons();
      }
    }
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true
  });
}

// Verify a specific code block
async function verifyCodeBlock(block) {
  const code = block.textContent;
  const language = detectLanguageFromElement(block) || detectLanguage(code);

  showNotification('Verifying code...', 'info');

  try {
    // Try to connect to local daemon
    const daemonUrl = (await chrome.storage.local.get(['daemonUrl'])).daemonUrl || 'http://localhost:9847';

    const response = await fetch(`${daemonUrl}/api/verify`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        code,
        language,
        project: projectContext?.name
      })
    });

    if (!response.ok) throw new Error('Verification request failed');

    const result = await response.json();
    showVerificationResult(result, block);

  } catch (e) {
    console.error('[Claude Verify] Verification failed:', e);
    showNotification('Verification failed - is the daemon running?', 'error');

    // Show offline indicator
    block.dataset.cvStatus = 'unknown';
    addStatusIndicator(block, 'unknown', 'Could not verify - daemon not connected');
  }
}

// Show verification result
function showVerificationResult(result, block = null) {
  if (!block) {
    // Find the most recent code block
    const blocks = document.querySelectorAll('pre[data-cv-processed]');
    block = blocks[blocks.length - 1];
  }

  if (!block) return;

  const status = result.status || 'unknown';
  const confidence = result.confidence || 0;
  const message = result.message || statusToMessage(status);

  block.dataset.cvStatus = status;
  addStatusIndicator(block, status, `${message} (${Math.round(confidence * 100)}% confidence)`);

  // Show notification
  const notifType = status === 'proved' ? 'success' :
                    status === 'counterexample' ? 'error' : 'warning';
  showNotification(message, notifType);
}

// Add status indicator to code block
function addStatusIndicator(block, status, tooltip) {
  // Remove existing indicator
  const existing = block.querySelector('.cv-status-indicator');
  if (existing) existing.remove();

  const indicator = document.createElement('div');
  indicator.className = `cv-status-indicator cv-status-${status}`;
  indicator.title = tooltip;

  const icon = status === 'proved' ? '✓' :
               status === 'counterexample' ? '✗' : '?';
  indicator.textContent = icon;

  block.appendChild(indicator);
}

// Report an issue with code
function reportCodeIssue(block) {
  const code = block.textContent;

  // Create feedback form overlay
  const overlay = document.createElement('div');
  overlay.className = 'cv-feedback-overlay';
  overlay.innerHTML = `
    <div class="cv-feedback-form">
      <h3>Report Issue</h3>
      <p>What's wrong with this code?</p>
      <textarea class="cv-feedback-issue" placeholder="Describe the issue..." rows="3"></textarea>
      <p>What should it be instead? (optional)</p>
      <textarea class="cv-feedback-correction" placeholder="Corrected code..." rows="3"></textarea>
      <div class="cv-feedback-actions">
        <button class="cv-btn cv-btn-primary cv-submit-feedback">Submit</button>
        <button class="cv-btn cv-cancel-feedback">Cancel</button>
      </div>
    </div>
  `;

  document.body.appendChild(overlay);

  // Handle submit
  overlay.querySelector('.cv-submit-feedback').onclick = async () => {
    const issue = overlay.querySelector('.cv-feedback-issue').value;
    const correction = overlay.querySelector('.cv-feedback-correction').value;

    if (!issue.trim()) {
      alert('Please describe the issue');
      return;
    }

    // Store feedback
    const feedback = {
      output: code,
      issue: issue,
      correction: correction,
      project: projectContext?.name,
      timestamp: new Date().toISOString()
    };

    const stored = await chrome.storage.local.get(['feedbackItems']);
    const items = stored.feedbackItems || [];
    items.push(feedback);
    await chrome.storage.local.set({ feedbackItems: items });

    // Try to send to daemon
    try {
      const daemonUrl = (await chrome.storage.local.get(['daemonUrl'])).daemonUrl || 'http://localhost:9847';
      await fetch(`${daemonUrl}/api/feedback`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(feedback)
      });
    } catch (e) {
      console.log('[Claude Verify] Could not send feedback to daemon');
    }

    overlay.remove();
    showNotification('Feedback recorded!', 'success');
  };

  // Handle cancel
  overlay.querySelector('.cv-cancel-feedback').onclick = () => {
    overlay.remove();
  };
}

// Show notification
function showNotification(message, type = 'info') {
  // Remove existing
  const existing = document.querySelector('.cv-notification');
  if (existing) existing.remove();

  const notification = document.createElement('div');
  notification.className = `cv-notification cv-notification-${type}`;
  notification.textContent = message;

  document.body.appendChild(notification);

  // Auto-remove after 3 seconds
  setTimeout(() => {
    notification.classList.add('cv-notification-fade');
    setTimeout(() => notification.remove(), 300);
  }, 3000);
}

// Status to message
function statusToMessage(status) {
  switch (status) {
    case 'proved': return 'Code verified correct';
    case 'counterexample': return 'Issue found in code';
    case 'unknown': return 'Could not verify';
    case 'error': return 'Verification error';
    default: return 'Unknown status';
  }
}
