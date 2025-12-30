// FFI bindings for GitHub API

export const fetchLatestMergedPRImpl = (owner) => (repo) => () => {
  // Fetch the latest merged pull requests from GitHub API
  // We look for merged PRs (state=closed, is:merged)
  const url = `https://api.github.com/repos/${owner}/${repo}/pulls?state=closed&per_page=10&sort=updated&direction=desc`;

  return fetch(url)
    .then(response => {
      if (!response.ok) {
        throw new Error(`GitHub API returned ${response.status}`);
      }
      return response.json();
    })
    .then(pulls => {
      // Find the first merged PR
      const mergedPR = pulls.find(pr => pr.merged_at !== null);
      if (mergedPR) {
        return mergedPR.number;
      }
      return null;
    })
    .catch(error => {
      console.warn('Failed to fetch latest merged PR:', error);
      return null;
    });
};
