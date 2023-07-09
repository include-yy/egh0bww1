/* Start of copy code */
// based on https://www.roboleary.net/2022/01/13/copy-code-to-clipboard-blog.html
const copyLabel = 'Copy code';

async function copyCode(block, button) {
    let code = block.querySelector('pre.src');
    let text = code.innerText;
    await navigator.clipboard.writeText(text);
    button.innerText = 'Copied';
    setTimeout(() => {
        button.innerText = copyLabel;
    }, 500);
}

function addCopyCodeButtons() {
    if (!navigator.clipboard) return;
    let blocks = document.querySelectorAll('.org-src-container');
    blocks.forEach((block) => {
        let button = document.createElement('button');
        button.innerText = copyLabel;
        button.classList.add('copy-code');
	button.type = 'button'
        let details = block.closest('details');
        let summary = details && details.querySelector('summary');
        if (summary) {
            summary.appendChild(button);
        } else {
            block.appendChild(button);
        }
        button.addEventListener('click', async() => {
            await copyCode(block, button);
        });
        block.setAttribute('tabindex', 0);
    });
}
document.addEventListener("DOMContentLoaded", function(event) {
    addCopyCodeButtons();
});
/* End of copy code */
